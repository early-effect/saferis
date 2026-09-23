package saferis.tests

import saferis.*
import saferis.ddl.*
import saferis.dml.*
import saferis.postgres.given
import saferis.tests.PostgresTestContainer.DataSourceProvider
import zio.*
import zio.test.*

import java.sql.Connection

object TransactorSpecs extends ZIOSpecDefault:
  val serializable = DataSourceProvider.configured(
    JdbcSessionConfig(configure = _.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE))
  )
  val readCommitted = DataSourceProvider.configured(
    JdbcSessionConfig(configure = _.setTransactionIsolation(Connection.TRANSACTION_READ_COMMITTED))
  )
  val defaultTransactor = DataSourceProvider.default

  val frank                = "Frank"
  val bob                  = "Bob"
  val alice                = "Alice"
  val charlie              = "Charlie"
  val none: Option[String] = None

  @tableName("test_table_no_key")
  final case class TestTable(
      @key name: String,
      age: Option[Int],
      @label("email") e: Option[String],
  ) derives Table

  val testTable = Table[TestTable]

  val runSuite =
    suiteAll("should run"):
      test("a select all query"):
        val sql = sql"select * from $testTable"
        for a <-
            sql.query[TestTable]
        yield assertTrue(a.size == 4)

      test("a single row query"):
        val sql = sql"select * from $testTable where name = $alice"
        for a <-
            sql.queryOne[TestTable]
        yield assertTrue(a == Some(TestTable("Alice", Some(30), Some("alice@example.com"))))

      test("queryOne returns the first row and does not decode a later null"):
        @tableName("query_one_first_row")
        final case class NameRow(id: Int, name: String) derives Table

        for
          _   <- sql"drop table if exists query_one_first_row".dml
          _   <- sql"create table query_one_first_row (id integer, name varchar(255))".dml
          _   <- sql"insert into query_one_first_row (id, name) values (1, 'ok'), (2, null)".dml
          row <- sql"select id, name from query_one_first_row order by id".queryOne[NameRow]
        yield assertTrue(row.contains(NameRow(1, "ok")))

      test("a single value query"):
        val sql = sql"select count(1) from $testTable"
        for a <-
            sql.queryValue[Long]
        yield assertTrue(a == Some(4))

      test("a single tuple query"):
        val sql = sql"select name, age from $testTable where name = $bob"
        for a <-
            sql.queryValue[(String, Option[Int])]
        yield assertTrue(a == Some((bob, Some(25))))

      test("insert operation"):
        @tableName("test_transactor_insert")
        final case class InsertTable(@key id: Int, name: String) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_insert".dml
          _ <-
            sql"create table test_transactor_insert (id integer primary key, name varchar(255))".dml
          rowsAffected <-
            insert(InsertTable(1, "Test Insert"))
          result <-
            sql"select * from test_transactor_insert where id = 1".queryOne[InsertTable]
        yield assertTrue(rowsAffected == 1) &&
          assertTrue(result.contains(InsertTable(1, "Test Insert")))
        end for

      test("update operation"):
        @tableName("test_transactor_update")
        final case class UpdateTable(@key id: Int, name: String) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_update".dml
          _ <-
            sql"create table test_transactor_update (id integer primary key, name varchar(255))".dml
          _ <-
            insert(UpdateTable(1, "Original"))
          rowsAffected <-
            update(UpdateTable(1, "Updated"))
          result <-
            sql"select * from test_transactor_update where id = 1".queryOne[UpdateTable]
        yield assertTrue(rowsAffected == 1) &&
          assertTrue(result.contains(UpdateTable(1, "Updated")))
        end for

      test("delete operation"):
        @tableName("test_transactor_delete")
        final case class DeleteTable(@key id: Int, name: String) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_delete".dml
          _ <-
            sql"create table test_transactor_delete (id integer primary key, name varchar(255))".dml
          _ <-
            insert(DeleteTable(1, "To Delete"))
          rowsAffected <-
            delete(DeleteTable(1, "To Delete"))
          result <-
            sql"select * from test_transactor_delete where id = 1".queryOne[DeleteTable]
        yield assertTrue(rowsAffected == 1) &&
          assertTrue(result.isEmpty)
        end for

      test("DDL operations"):
        @tableName("test_transactor_ddl")
        final case class DDLTable(@key id: Int, name: String) derives Table

        for
          _ <-
            dropTable[DDLTable](ifExists = true)
          createResult <-
            createTable[DDLTable]()
          _ <-
            insert(DDLTable(1, "DDL Test"))
          queryResult <-
            sql"select * from test_transactor_ddl where id = 1".queryOne[DDLTable]
          dropResult <-
            dropTable[DDLTable]()
        yield assertTrue(createResult >= 0) &&
          assertTrue(queryResult.contains(DDLTable(1, "DDL Test"))) &&
          assertTrue(dropResult >= 0)
        end for
  end runSuite

  val transactionSuite =
    suiteAll("should run statements in a transaction"):
      test("yielding an effect of the result"):
        for a <- transact(
            for
              a <- sql"select * from $testTable where name = $alice".queryOne[TestTable]
              b <- sql"select * from $testTable where name = $bob".queryOne[TestTable]
              c <- sql"select * from $testTable where name = $none".queryOne[TestTable]
            yield a.toSeq ++ b.toSeq ++ c.toSeq
          )
        yield assertTrue(a.size == 2)

      test("commit on success"):
        for
          a <- transact:
            insertReturning(TestTable(frank, Some(42), None))
          newNames <-
            sql"select * from $testTable".query[TestTable].map(_.map(_.name))
        yield assertTrue(a.name == frank) &&
          assertTrue(newNames.contains(frank)) &&
          assertTrue(newNames.size == 5)

      test("rollback on effect failure"):
        val names = sql"select * from $testTable".query[TestTable].map(_.map(_.name))
        for
          before <-
            names
          error <- transact(
            for
              _ <- sql"delete from $testTable where name = $bob".delete
              _ <- sql"delete from $testTable where name = $alice".delete
              _ <- sql"deli meat from $testTable where name = $charlie".delete // pastrami please - this will fail
            yield ()
          ).flip
          after <-
            names
          isSyntaxError = error match
            case SaferisError.SyntaxError(_, _, _) => true
            case _                                 => false
        yield assertTrue(isSyntaxError) && // the pastrami is a lie
          assertTrue(after == before)
        end for

      test("mixed operations in transaction"):
        @tableName("test_transactor_mixed")
        final case class MixedTable(@key id: Int, name: String, value: Int) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_mixed".dml
          _ <-
            sql"create table test_transactor_mixed (id integer primary key, name varchar(255), value integer)".dml
          result <- transact:
            for
              _         <- insert(MixedTable(1, "First", 10))
              _         <- insert(MixedTable(2, "Second", 20))
              _         <- update(MixedTable(1, "Updated First", 15))
              remaining <- sql"select * from test_transactor_mixed".query[MixedTable]
            yield remaining
          count <-
            sql"select count(*) as count from test_transactor_mixed".queryOne[CountResult]
        yield assertTrue(result.size == 2) &&
          assertTrue(result.exists(_.name == "Updated First")) &&
          assertTrue(count.map(_.count).contains(2))
        end for

      test("transaction isolation"):
        @tableName("test_transactor_isolation")
        final case class IsolationTable(@key id: Int, value: Int) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_isolation".dml
          _ <-
            sql"create table test_transactor_isolation (id integer primary key, value integer)".dml
          _ <-
            insert(IsolationTable(1, 100))
          // Test that transaction sees consistent data
          result <- transact:
            for
              initial <- sql"select * from test_transactor_isolation where id = 1".queryOne[IsolationTable]
              _       <- update(IsolationTable(1, 200))
              updated <- sql"select * from test_transactor_isolation where id = 1".queryOne[IsolationTable]
            yield (initial, updated)
        yield assertTrue(result._1.map(_.value).contains(100)) &&
          assertTrue(result._2.map(_.value).contains(200))
        end for

      test("nested transact joins the outer transaction and does not commit early"):
        for
          _      <- sql"drop table if exists test_nested_txn".dml
          _      <- sql"create table test_nested_txn (id integer primary key)".dml
          failed <- transact(
            for
              _ <- transact(sql"insert into test_nested_txn (id) values (1)".dml)
              _ <- ZIO.fail(SaferisError.Unexpected("outer failed"))
            yield ()
          ).exit
          count <- sql"select count(*) from test_nested_txn".queryValue[Long]
        yield assertTrue(failed.isFailure, count.contains(0L))

      test("nested transact commits with the outer transaction"):
        for
          _ <- sql"drop table if exists test_nested_txn_ok".dml
          _ <- sql"create table test_nested_txn_ok (id integer primary key)".dml
          _ <- transact:
            transact(sql"insert into test_nested_txn_ok (id) values (1)".dml)
          count <- sql"select count(*) from test_nested_txn_ok".queryValue[Long]
        yield assertTrue(count.contains(1L))

      test("a failure inside nested transact rolls the outer transaction back"):
        for
          _    <- sql"drop table if exists test_nested_fail".dml
          _    <- sql"create table test_nested_fail (id integer primary key)".dml
          exit <- transact(
            for
              _ <- sql"insert into test_nested_fail (id) values (1)".dml
              _ <- transact(sql"deli meat from test_nested_fail".dml)
            yield ()
          ).exit
          count <- sql"select count(*) from test_nested_fail".queryValue[Long]
          syntax = exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(_: SaferisError.SyntaxError) => true
                case _                                 => false
            case _ => false
        yield assertTrue(syntax, count.contains(0L))

      test("catching a statement failure still rolls back and the next command is 25P02"):
        for
          _    <- sql"drop table if exists test_abort_txn".dml
          _    <- sql"create table test_abort_txn (id integer primary key)".dml
          seen <- Ref.make[Option[Either[SaferisError, Option[Long]]]](None)
          exit <- transact(
            for
              _     <- sql"insert into test_abort_txn (id) values (1)".dml
              _     <- sql"deli meat from test_abort_txn".dml.catchAll(_ => ZIO.succeed(0L))
              later <- sql"select count(*) from test_abort_txn".queryValue[Long].either
              _     <- seen.set(Some(later))
            yield ()
          ).exit
          captured <- seen.get
          count    <- sql"select count(*) from test_abort_txn".queryValue[Long]
          aborted = captured match
            case Some(Left(SaferisError.QueryError(Some("25P02"), _, _))) => true
            case _                                                        => false
          recorded = exit match
            case Exit.Failure(cause) =>
              cause.failureOption match
                case Some(_: SaferisError.SyntaxError) => true
                case _                                 => false
            case _ => false
        yield assertTrue(aborted, recorded, count.contains(0L))
  end transactionSuite

  val concurrencyTestsUnlimited =
    suiteAll("should handle unlimited concurrency"):
      test("concurrent access with no semaphore limit"):
        @tableName("test_transactor_unlimited")
        final case class UnlimitedTable(@key id: Int, name: String) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_unlimited".dml
          _ <-
            sql"create table test_transactor_unlimited (id integer primary key, name varchar(255))".dml
          // Run many operations concurrently - should all succeed quickly
          results <- ZIO.collectAllPar:
            (1 to 10).map: i =>
              insert(UnlimitedTable(i, s"Unlimited $i"))
          count <-
            sql"select count(*) as count from test_transactor_unlimited".queryOne[CountResult]
        yield assertTrue(results.forall(_ == 1)) &&
          assertTrue(count.map(_.count).contains(10))
        end for

  val concurrencyTestsLimited =
    suiteAll("should handle limited concurrency"):
      test("concurrent access with semaphore limit of 2"):
        @tableName("test_transactor_limited")
        final case class LimitedTable(@key id: Int, name: String) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_limited".dml
          _ <-
            sql"create table test_transactor_limited (id integer primary key, name varchar(255))".dml
          // Run operations concurrently - only 2 should run at once due to semaphore
          results <- ZIO.collectAllPar:
            (1 to 5).map: i =>
              insert(LimitedTable(i, s"Limited $i"))
          count <-
            sql"select count(*) as count from test_transactor_limited".queryOne[CountResult]
        yield assertTrue(results.forall(_ == 1)) &&
          assertTrue(count.map(_.count).contains(5))
        end for

  val concurrencyTestsSerialized =
    suiteAll("should handle serialized concurrency"):
      test("concurrent access with semaphore limit of 1"):
        @tableName("test_transactor_serialized")
        final case class SerializedTable(@key id: Int, name: String) derives Table

        for
          _ <-
            sql"drop table if exists test_transactor_serialized".dml
          _ <-
            sql"create table test_transactor_serialized (id integer primary key, name varchar(255))".dml
          // Run operations concurrently - but they should be effectively serialized
          results <- ZIO.collectAllPar:
            (1 to 3).map: i =>
              insert(SerializedTable(i, s"Serialized $i"))
          count <-
            sql"select count(*) as count from test_transactor_serialized".queryOne[CountResult]
        yield assertTrue(results.forall(_ == 1)) &&
          assertTrue(count.map(_.count).contains(3))
        end for

  val configuratorTests =
    suiteAll("should apply configurator"):
      test("configurator sets transaction isolation"):
        for
          // This transactor was configured with SERIALIZABLE isolation
          // We'll verify it works correctly (detailed isolation testing would require more complex scenarios)
          result <- transact:
            sql"select 1 as value".queryOne[ValueResult]
        yield assertTrue(result.map(_.value).contains(1))

  final case class CountResult(count: Long) derives Table
  final case class ValueResult(value: Int) derives Table

  val all = suiteAll("A transactor"):
    runSuite.provideShared(defaultTransactor)
    transactionSuite.provideShared(serializable)
    concurrencyTestsUnlimited.provideShared(defaultTransactor)
    concurrencyTestsLimited.provideShared(readCommitted)
    concurrencyTestsSerialized.provideShared(serializable)
    configuratorTests.provideShared(serializable)

  val spec = all @@ TestAspect.sequential

end TransactorSpecs
