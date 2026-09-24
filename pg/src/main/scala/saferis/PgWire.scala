package saferis.pg

import saferis.SaferisError
import saferis.SqlCommand
import saferis.SqlPiece
import saferis.SqlRow
import saferis.SqlType
import saferis.SqlValue
import saferis.ServerType
import saferis.postgres.PgConnectionConfig
import saferis.postgres.PgText
import saferis.postgres.SslMode

import zio.Chunk
import zio.Config.Secret
import zio.Duration

import scala.scalajs.js

/** Node glue around [[PgText]]: `js.Any` bind values, pool options, and cursor row assembly. */
private[pg] object PgWire:
  private val rawParser: js.Function1[js.Any, js.Any] =
    (value: js.Any) => value

  /** Returns the raw text for every OID. Passed on the pool and on the cursor. Not `pg.types.setTypeParser`. */
  val getTypeParser: js.Function2[js.Any, js.Any, js.Function1[js.Any, js.Any]] =
    (_: js.Any, _: js.Any) => rawParser

  val rawTypes: js.Object =
    js.Dynamic.literal("getTypeParser" -> getTypeParser)

  /** `rowMode` stays `array` so the OID decode does not change. Batch size is the `read` argument. */
  def cursorConfig: js.Object =
    js.Dynamic.literal(
      "rowMode" -> "array",
      "types"   -> rawTypes,
    )

  def poolConfig(config: PgConfig): js.Object =
    val connection = config.connection
    js.Dynamic.literal(
      "host"                    -> connection.host,
      "port"                    -> connection.port.toDouble,
      "database"                -> connection.database,
      "user"                    -> connection.user,
      "password"                -> reveal(connection.password),
      "max"                     -> config.poolSize.toDouble,
      "ssl"                     -> sslValue(connection.ssl),
      "connectionTimeoutMillis" -> connection.connectTimeout.toMillis.toDouble,
      "options"                 -> startupOptions(connection),
      "allowExitOnIdle"         -> true,
      "types"                   -> rawTypes,
    )
  end poolConfig

  def startupOptions(connection: PgConnectionConfig): String =
    val extra = connection.parameters.map { (name, value) => s"-c $name=$value" }.mkString(" ")
    if extra.isEmpty then "-c DateStyle=ISO" else s"-c DateStyle=ISO $extra"

  def reveal(secret: Secret): String =
    secret.value.mkString

  /** `undefined` tells Node TLS not to check the certificate hostname. */
  private val skipHostname: js.Function2[js.Any, js.Any, js.UndefOr[js.Any]] =
    (_, _) => js.undefined

  def sslValue(mode: SslMode): js.Any = mode match
    case SslMode.Disable =>
      false.asInstanceOf[js.Any]
    case SslMode.Require =>
      js.Dynamic.literal(rejectUnauthorized = false)
    case SslMode.VerifyCa(ca) =>
      // rejectUnauthorized alone is verify-full. verify-ca skips the hostname check.
      js.Dynamic.literal(
        rejectUnauthorized = true,
        ca = ca.text,
        checkServerIdentity = skipHostname,
      )
    case SslMode.VerifyFull(ca) =>
      js.Dynamic.literal(rejectUnauthorized = true, ca = ca.text)

  def queryConfig(sql: String, values: js.Array[js.Any]): js.Object =
    js.Dynamic.literal(
      "text"    -> sql,
      "values"  -> values,
      "rowMode" -> "array",
    )

  def render(command: SqlCommand): String =
    command.render: (index, tpe) =>
      PgText.cast(tpe) match
        case Some(name) => s"$$$index::$name"
        case None       => s"$$$index"

  /** Whole milliseconds, round up, minimum 1. Infinity is `Int.MaxValue` milliseconds. `0` is not a present cap. */
  def millis(d: Duration): Int =
    if d.compareTo(Duration.Infinity) >= 0 then Int.MaxValue
    else if d.isZero || d.isNegative then 1
    else
      val seconds = d.getSeconds
      val nano    = d.getNano
      if seconds >= (Int.MaxValue.toLong / 1000L) then Int.MaxValue
      else
        val base    = seconds * 1000L + (nano / 1000000L)
        val rounded = if nano % 1000000L != 0L then base + 1L else base
        if rounded < 1L then 1
        else if rounded > Int.MaxValue.toLong then Int.MaxValue
        else rounded.toInt

  def parameters(pieces: Chunk[SqlPiece]): js.Array[js.Any] =
    val values = js.Array[js.Any]()
    pieces.foreach:
      case SqlPiece.Text(_)      => ()
      case SqlPiece.Param(value) =>
        val _ = values.push(bind(value))
    values

  def ensureSingle(result: PgResult): Either[SaferisError, PgResult] =
    if js.Array.isArray(result.asInstanceOf[js.Any]) then
      Left(SaferisError.Unexpected("multiple statements are not one command"))
    else Right(result)

  def rowCount(result: PgResult): Either[SaferisError, Long] =
    ensureSingle(result).map: single =>
      val raw = single.asInstanceOf[js.Dynamic].rowCount
      if js.isUndefined(raw) || (raw eq null) then 0L
      else raw.asInstanceOf[Double].toLong

  def readRows(result: PgResult): Either[SaferisError, Chunk[SqlRow]] =
    ensureSingle(result).flatMap: single =>
      readFields(single)

  /** One portal row. `fields` come from the cursor result, not from a buffered `PgResult`. */
  def readCursorRow(fields: js.Array[PgField], row: js.Array[js.Any]): Either[SaferisError, SqlRow] =
    if fields == null || js.isUndefined(fields) then Left(SaferisError.Unexpected("cursor result has no fields"))
    else
      val labels = Chunk.fromIterator(Iterator.tabulate(fields.length)(i => fieldName(fields(i))))
      val oids   = Array.tabulate(fields.length)(i => fields(i).dataTypeID.toInt)
      readRow(labels, oids, row)

  private def readFields(result: PgResult): Either[SaferisError, Chunk[SqlRow]] =
    val fields = result.fields
    if fields == null || js.isUndefined(fields) then Left(SaferisError.Unexpected("query result has no fields"))
    else
      val width  = fields.length
      val labels = Chunk.fromIterator(Iterator.tabulate(width)(i => fieldName(fields(i))))
      val oids   = Array.tabulate(width)(i => fields(i).dataTypeID.toInt)
      val grid   = result.rows
      if grid == null || js.isUndefined(grid) then Right(Chunk.empty)
      else readGrid(labels, oids, grid)
  end readFields

  private def readGrid(
      labels: Chunk[String],
      oids: Array[Int],
      grid: js.Array[js.Array[js.Any]],
  ): Either[SaferisError, Chunk[SqlRow]] =
    val rows                         = Chunk.newBuilder[SqlRow]
    var index                        = 0
    var failed: Option[SaferisError] = None
    while index < grid.length && failed.isEmpty do
      readRow(labels, oids, grid(index)) match
        case Left(err)  => failed = Some(err)
        case Right(row) => rows += row
      index += 1
    failed.fold[Either[SaferisError, Chunk[SqlRow]]](Right(rows.result()))(Left(_))
  end readGrid

  private def readRow(
      labels: Chunk[String],
      oids: Array[Int],
      row: js.Array[js.Any],
  ): Either[SaferisError, SqlRow] =
    if row.length != oids.length then
      Left(SaferisError.DecodingError("", "row", s"width ${row.length} does not match ${oids.length} fields"))
    else
      val cells = (0 until oids.length).foldLeft[Either[SaferisError, Chunk[SqlValue]]](Right(Chunk.empty)):
        case (Left(err), _)  => Left(err)
        case (Right(acc), i) =>
          readCell(oids(i), labels(i), row(i)).map(acc :+ _)
      cells.map(values => SqlRow(labels, values))

  private def fieldName(field: PgField): String =
    val name = field.name
    if name == null then "" else name

  /** SQL null is `Null` before any JSON parse. JSON null arrives as the text `null`. */
  private def readCell(oid: Int, label: String, raw: js.Any): Either[SaferisError, SqlValue] =
    if isJsNull(raw) then Right(SqlValue.Null(PgText.sqlType(oid).getOrElse(SqlType.Other(ServerType.Oid(oid)))))
    else if js.typeOf(raw) != "string" then
      Left(SaferisError.DecodingError(label, oid.toString, s"expected raw text for oid $oid"))
    else
      PgText
        .decode(oid, raw.asInstanceOf[String])
        .left
        .map: detail =>
          SaferisError.DecodingError(label, PgText.typeLabel(oid), detail)

  private def bind(value: SqlValue): js.Any = value match
    case SqlValue.Null(_)          => jsNull
    case SqlValue.Bool(v)          => js.Any.fromBoolean(v)
    case SqlValue.Int2(v)          => js.Any.fromDouble(v.toDouble)
    case SqlValue.Int4(v)          => js.Any.fromDouble(v.toDouble)
    case SqlValue.Int8(v)          => v.toString
    case SqlValue.Float4(v)        => js.Any.fromDouble(v.toDouble)
    case SqlValue.Float8(v)        => js.Any.fromDouble(v)
    case SqlValue.Array(_, values) => js.Array(values.map(bind)*)
    case other                     =>
      PgText.encode(other) match
        case Some(text) => text
        case None       => jsNull

  /** JS null. Stays inside the bind. Callers see `SqlValue.Null`, not this. */
  private def jsNull: js.Any = null

  private def isJsNull(raw: js.Any): Boolean =
    js.isUndefined(raw) || (raw eq null)
end PgWire
