package saferis

import zio.*
import zio.test.*

/** `SqlSession.pooled` against a connection that records calls. No database and no driver. */
object ScriptedSessionSpecs extends ZIOSpecDefault:
  def spec = suite("scripted SqlConnection")(
    test("nested transact begins and commits once"):
      for
        log <- Ref.make(Chunk.empty[String])
        _   <- run(log):
          transact(transact(sql"select inner".execute) *> sql"select outer".execute)
        calls <- log.get
      yield assertTrue(
        calls.count(_ == "begin") == 1,
        calls.count(_ == "commit") == 1,
        calls.count(_.startsWith("exec:")) == 2,
      )
    ,
    test("catching a statement failure sends the next command and commits"):
      for
        log  <- Ref.make(Chunk.empty[String])
        exit <- run(log):
          transact(sql"boom".execute.catchAll(_ => ZIO.succeed(0L)) *> sql"select later".execute).exit
        calls <- log.get
      yield assertTrue(
        exit.isSuccess,
        calls.exists(_.contains("boom")),
        calls.exists(_.contains("later")),
        calls.contains("commit"),
        !calls.contains("rollback"),
      )
    ,
    test("a failed body rolls back and does not commit"):
      for
        log   <- Ref.make(Chunk.empty[String])
        exit  <- run(log)(transact(sql"select 1".execute *> ZIO.fail(SaferisError.Unexpected("no"))).exit)
        calls <- log.get
      yield assertTrue(exit.isFailure, calls.contains("rollback"), !calls.contains("commit"))
    ,
    test("a failed commit fails the transaction and does not report success"):
      for
        log   <- Ref.make(Chunk.empty[String])
        exit  <- run(log, failCommit = true)(transact(sql"select 1".execute).exit)
        calls <- log.get
        failed = exit match
          case Exit.Failure(cause) =>
            cause.failureOption match
              case Some(SaferisError.QueryError(detail)) =>
                detail.sqlState.contains(SqlState.SerializationFailure)
              case _ => false
          case _ => false
      yield assertTrue(failed, calls.contains("commit"))
    ,
    test("interrupting the body rolls back and does not commit"):
      for
        log <- Ref.make(Chunk.empty[String])
        script = new Script(log, failCommit = false)
        fiber <- transact(ZIO.never).provide(ZLayer.succeed(SqlSession.pooled(ZIO.succeed(script)))).fork
        _     <- log.get.repeatUntil(_.contains("begin"))
        _     <- fiber.interrupt
        calls <- log.get
      yield assertTrue(calls.contains("begin"), calls.contains("rollback"), !calls.contains("commit"))
    ,
    test("catching a failed stream commits"):
      for
        log <- Ref.make(Chunk.empty[String])
        script = new Script(log, failCommit = false, failCursor = true)
        exit <- transact(
          ZIO.serviceWithZIO[SqlSession]: session =>
            val read: SqlRow => Either[SaferisError, Int] = _ => Right(1)
            session
              .stream(SqlCommand(Chunk(SqlPiece.Text(SqlText("stream-boom"))), None))(read)
              .runDrain
              .catchAll(_ => ZIO.unit)
        ).provide(ZLayer.succeed(SqlSession.pooled(ZIO.succeed(script)))).exit
        calls <- log.get
      yield assertTrue(
        exit.isSuccess,
        calls.exists(_.startsWith("cursor:")),
        calls.contains("commit"),
        !calls.contains("rollback"),
      )
    ,
    test("an uncaught stream failure rolls back and does not commit"):
      for
        log <- Ref.make(Chunk.empty[String])
        script = new Script(log, failCommit = false, failCursor = true)
        exit <- transact(
          ZIO.serviceWithZIO[SqlSession]: session =>
            val read: SqlRow => Either[SaferisError, Int] = _ => Right(1)
            session.stream(SqlCommand(Chunk(SqlPiece.Text(SqlText("stream-boom"))), None))(read).runDrain
        ).provide(ZLayer.succeed(SqlSession.pooled(ZIO.succeed(script)))).exit
        calls <- log.get
      yield assertTrue(
        exit.isFailure,
        calls.exists(_.startsWith("cursor:")),
        calls.contains("rollback"),
        !calls.contains("commit"),
      ),
  )

  private def run[A](log: Ref[Chunk[String]], failCommit: Boolean = false)(
      body: ZIO[SqlSession, SaferisError, A]
  ): ZIO[Any, SaferisError, A] =
    body.provide(ZLayer.succeed(SqlSession.pooled(ZIO.succeed(new Script(log, failCommit)))))

  private final class Script(log: Ref[Chunk[String]], failCommit: Boolean, failCursor: Boolean = false)
      extends SqlConnection:
    def execute(command: SqlCommand): IO[SaferisError, Long] =
      note(s"exec:${command.inspection}") *>
        ZIO
          .when(command.inspection.contains("boom"))(
            ZIO.fail(
              SaferisError.SyntaxError(
                ServerDetail("boom", Some(command.inspection), SqlState.parse("42601"), None, None)
              )
            )
          )
          .unit
          .as(1L)

    def query(command: SqlCommand): IO[SaferisError, Chunk[SqlRow]] =
      note(s"query:${command.inspection}").as(Chunk.empty)

    def queryAtMostOne(command: SqlCommand): IO[SaferisError, Option[SqlRow]] =
      note(s"one:${command.inspection}").as(None)

    def cursor(command: SqlCommand): zio.stream.ZStream[Any, SaferisError, SqlRow] =
      zio.stream.ZStream.unwrap:
        note(s"cursor:${command.inspection}").as:
          if failCursor then
            zio.stream.ZStream.succeed(SqlRow(Chunk(ColumnName("n")), Chunk(SqlValue.Integer(1)))) ++
              zio.stream.ZStream.fail(
                SaferisError.QueryError(
                  ServerDetail("cursor failed", Some(command.inspection), SqlState.parse("42000"), None, None)
                )
              )
          else zio.stream.ZStream.empty

    def begin: IO[SaferisError, Unit] =
      note("begin")

    def commit: IO[SaferisError, Unit] =
      note("commit") *>
        ZIO
          .when(failCommit)(
            ZIO.fail(
              SaferisError.QueryError(
                ServerDetail("commit failed", None, Some(SqlState.SerializationFailure), None, None)
              )
            )
          )
          .unit

    def rollback: UIO[Unit] =
      note("rollback")

    private def note(event: String): UIO[Unit] =
      log.update(_ :+ event)
  end Script
end ScriptedSessionSpecs
