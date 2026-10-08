package saferis

/** Why a cell is not the Scala value a decoder asked for. `SaferisError.DecodingError` carries one of these. */
enum DecodeError:
  case Null
  case Mismatch(expected: TypeName, found: SqlValue)
  case OutOfRange(expected: TypeName, found: SqlValue)
  case Lossy(expected: TypeName, found: SqlValue)
  case InvalidText(expected: TypeName, text: String)
  case MissingColumn(label: ColumnName)
  case IndexOutOfRange(index: Int, width: Int)
  case Width(expected: Int, actual: Int)
  case At(index: Int, reason: DecodeError)
  case MissingField(owner: String, field: String)
  case Arity(owner: String, expected: Int, actual: Int)
  case UnknownLabel(typeName: TypeName, text: String)
  case Json(detail: String)
  case Unparsed(detail: String)

  def message: String = this match
    case Null                          => "null value"
    case Mismatch(expected, found)     => s"expected $expected, found ${found.productPrefix}"
    case OutOfRange(expected, found)   => s"${shown(found)} does not fit in $expected"
    case Lossy(expected, found)        => s"${shown(found)} is not exactly a $expected"
    case InvalidText(expected, text)   => s"'$text' is not a $expected"
    case MissingColumn(label)          => s"missing column $label"
    case IndexOutOfRange(index, width) => s"column index $index out of range (width $width)"
    case Width(expected, actual)       => s"Expected exactly $expected columns in result set, got $actual"
    case At(index, reason)             => s"column $index: ${reason.message}"
    case MissingField(owner, field)    =>
      s"Error constructing instance of $owner. Could not find value for parameter $field"
    case Arity(owner, expected, actual) =>
      s"wrong arity constructing $owner: expected $expected values, got $actual"
    case UnknownLabel(typeName, text) => s"'$text' is not a $typeName"
    case Json(detail)                 => s"Failed to decode JSON: $detail"
    case Unparsed(detail)             => detail

  private def shown(value: SqlValue): String = value match
    case SqlValue.SmallInt(v)        => v.toString
    case SqlValue.Integer(v)         => v.toString
    case SqlValue.BigInt(v)          => v.toString
    case SqlValue.Real(v)            => v.toString
    case SqlValue.DoublePrecision(v) => v.toString
    case SqlValue.Numeric(v)         => v.bigDecimal.toPlainString
    case SqlValue.VarChar(v)         => v
    case SqlValue.Text(v)            => v
    case SqlValue.Bool(v)            => v.toString
    case SqlValue.Date(v)            => v.toString
    case SqlValue.Uuid(v)            => v.toString
    case other                       => other.productPrefix
end DecodeError
