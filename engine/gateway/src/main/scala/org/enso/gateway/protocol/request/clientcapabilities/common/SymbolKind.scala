package org.enso.gateway.protocol.request.clientcapabilities.common

import io.circe.Decoder

/** A symbol kind. */
sealed abstract class SymbolKind(value: Int)
object SymbolKind {
  private val file              = 1
  private val module            = 2
  private val namespace         = 3
  private val packageKind       = 4
  private val classKind         = 5
  private val method            = 6
  private val property          = 7
  private val field             = 8
  private val constructor       = 9
  private val enum              = 10
  private val interface         = 11
  private val function          = 12
  private val variable          = 13
  private val constant          = 14
  private val string            = 15
  private val number            = 16
  private val boolean           = 17
  private val array             = 18
  private val objectKind        = 19
  private val key               = 20
  private val nullKind          = 21
  private val enumMember        = 22
  private val struct            = 23
  private val event             = 24
  private val operator          = 25
  private val typeParameter     = 26
  private val invalidSymbolKind = "Invalid SymbolKind"

  case object File extends SymbolKind(file)

  case object Module extends SymbolKind(module)

  case object Namespace extends SymbolKind(namespace)

  case object Package extends SymbolKind(packageKind)

  case object Class extends SymbolKind(classKind)

  case object Method extends SymbolKind(method)

  case object Property extends SymbolKind(property)

  case object Field extends SymbolKind(field)

  case object Constructor extends SymbolKind(constructor)

  case object Enum extends SymbolKind(enum)

  case object Interface extends SymbolKind(interface)

  case object Function extends SymbolKind(function)

  case object Variable extends SymbolKind(variable)

  case object Constant extends SymbolKind(constant)

  case object StringKind extends SymbolKind(string)

  case object Number extends SymbolKind(number)

  case object BooleanKind extends SymbolKind(boolean)

  case object ArrayKind extends SymbolKind(array)

  case object ObjectKind extends SymbolKind(objectKind)

  case object Key extends SymbolKind(key)

  case object NullKind extends SymbolKind(nullKind)

  case object EnumMember extends SymbolKind(enumMember)

  case object Struct extends SymbolKind(struct)

  case object Event extends SymbolKind(event)

  case object Operator extends SymbolKind(operator)

  case object TypeParameter extends SymbolKind(typeParameter)

  implicit val SymbolKindDecoder: Decoder[SymbolKind] =
    Decoder.decodeInt.emap {
      case `file`          => Right(File)
      case `module`        => Right(Module)
      case `namespace`     => Right(Namespace)
      case `packageKind`   => Right(Package)
      case `classKind`     => Right(Class)
      case `method`        => Right(Method)
      case `property`      => Right(Property)
      case `field`         => Right(Field)
      case `constructor`   => Right(Constructor)
      case `enum`          => Right(Enum)
      case `interface`     => Right(Interface)
      case `function`      => Right(Function)
      case `variable`      => Right(Variable)
      case `constant`      => Right(Constant)
      case `string`        => Right(StringKind)
      case `number`        => Right(Number)
      case `boolean`       => Right(BooleanKind)
      case `array`         => Right(ArrayKind)
      case `objectKind`    => Right(ObjectKind)
      case `key`           => Right(Key)
      case `nullKind`      => Right(NullKind)
      case `enumMember`    => Right(EnumMember)
      case `struct`        => Right(Struct)
      case `event`         => Right(Event)
      case `operator`      => Right(Operator)
      case `typeParameter` => Right(TypeParameter)
      case _               => Left(invalidSymbolKind)
    }
}
