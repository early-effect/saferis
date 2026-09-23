package saferis.tests

import saferis.*
import zio.test.*

import java.util.UUID

object MySQLCodecSpecs extends ZIOSpecDefault:

  val spec = suite("MySQL Codec Support")(
    test("MySQL UUID encoder uses CHAR(36) column type") {
      import saferis.mysql.given

      val dialect = summon[Dialect]
      val encoder = summon[Encoder[UUID]]

      val dialectName = dialect.name
      val colType     = encoder.columnType

      assertTrue(dialectName == "MySQL") &&
      assertTrue(colType == "char(36)") &&
      assertTrue(encoder.pgType == PgType.Text) &&
      assertTrue(dialect.columnType(PgType.Uuid) == "char(36)")
    },
    test("MySQL UUID encoder generates correct literal") {
      import saferis.mysql.given

      val uuid    = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
      val encoder = summon[Encoder[UUID]]
      val literal = encoder.literal(uuid)

      assertTrue(literal == "'550e8400-e29b-41d4-a716-446655440000'")
    },
    test("MySQL UUID decoder is available") {
      import saferis.mysql.given

      val decoder = summon[Decoder[UUID]]
      assertTrue(decoder != null)
    },
    test("Can create table with UUID column using MySQL dialect") {
      import saferis.mysql.given

      @tableName("mysql_users")
      final case class User(@key id: UUID, name: String) derives Table

      val table    = Table[User]
      val idColumn = table.id

      // The column should use MySQL's char(36) type
      val colType = idColumn.columnType

      assertTrue(colType == "char(36)")
    },
    test("PostgreSQL UUID codec still uses native uuid type") {
      // No MySQL import - uses default PostgreSQL dialect

      @tableName("pg_users")
      final case class User(@key id: UUID, name: String) derives Table

      val table    = Table[User]
      val idColumn = table.id

      // The column should use PostgreSQL's native uuid type
      val colType = idColumn.columnType

      assertTrue(colType == "uuid")
    },
  )
end MySQLCodecSpecs
