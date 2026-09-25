package saferis.sqlite.jdbc

import saferis.*
import saferis.sqlite.SQLiteDialect
import saferis.tests.Capability
import saferis.tests.DatabaseTarget
import saferis.tests.SqlSessionConformance

import org.sqlite.SQLiteDataSource
import zio.*
import zio.test.*

import java.nio.file.Files
import java.nio.file.Path
import javax.sql.DataSource

object SqliteConformanceSpecs extends ZIOSpecDefault:

  /** A fresh database file per run. The scope deletes it and its WAL files. */
  val dataSource: TaskLayer[DataSource] =
    ZLayer.scoped:
      ZIO
        .acquireRelease(ZIO.attemptBlocking(Files.createTempFile("saferis-sqlite", ".db"))): file =>
          ZIO.foreachDiscard(List("", "-wal", "-shm")): suffix =>
            ZIO.attemptBlocking(Files.deleteIfExists(Path.of(s"$file$suffix"))).ignore
        .map: file =>
          val ds = SQLiteDataSource()
          ds.setUrl(s"jdbc:sqlite:$file")
          ds

  val session: TaskLayer[SqlSession] = dataSource >>> SqliteJdbc.layer()

  /** SQLite reads its catalog through pragmas. It has no statement that runs long enough to cancel. */
  val target: ULayer[DatabaseTarget] = ZLayer.succeed(DatabaseTarget(SQLiteDialect, Set(Capability.Catalog)))

  /** SQLite proves itself by providing its session and target to the common suite, then runs what only SQLite does. */
  def spec =
    suite("sqlite")(
      SqlSessionConformance.suite,
      SqliteValueSpecs.spec,
    ).provideShared(session, target) @@ TestAspect.sequential
end SqliteConformanceSpecs
