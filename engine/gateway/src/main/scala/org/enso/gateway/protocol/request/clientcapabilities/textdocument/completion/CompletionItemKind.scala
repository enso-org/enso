package org.enso.gateway.protocol.request.clientcapabilities.textdocument.completion

import io.circe.Decoder

/** Kind of [[CompletionItem]]. */
sealed abstract class CompletionItemKind(value: Int)
object CompletionItemKind {
  private val text                      = 1
  private val method                    = 2
  private val function                  = 3
  private val constructor               = 4
  private val field                     = 5
  private val variable                  = 6
  private val classKind                 = 7
  private val interface                 = 8
  private val module                    = 9
  private val property                  = 10
  private val unit                      = 11
  private val value                     = 12
  private val enum                      = 13
  private val keyword                   = 14
  private val snippet                   = 15
  private val color                     = 16
  private val file                      = 17
  private val reference                 = 18
  private val folder                    = 19
  private val enumMember                = 20
  private val constant                  = 21
  private val struct                    = 22
  private val event                     = 23
  private val operator                  = 24
  private val typeParameter             = 25
  private val invalidCompletionItemKind = "Invalid CompletionItemKind"

  case object Text extends CompletionItemKind(text)

  case object Method extends CompletionItemKind(method)

  case object Function extends CompletionItemKind(function)

  case object Constructor extends CompletionItemKind(constructor)

  case object Field extends CompletionItemKind(field)

  case object Variable extends CompletionItemKind(variable)

  case object Class extends CompletionItemKind(classKind)

  case object Interface extends CompletionItemKind(interface)

  case object Module extends CompletionItemKind(module)

  case object Property extends CompletionItemKind(property)

  case object Unit extends CompletionItemKind(unit)

  case object Value extends CompletionItemKind(value)

  case object Enum extends CompletionItemKind(enum)

  case object Keyword extends CompletionItemKind(keyword)

  case object Snippet extends CompletionItemKind(snippet)

  case object Color extends CompletionItemKind(color)

  case object File extends CompletionItemKind(file)

  case object Reference extends CompletionItemKind(reference)

  case object Folder extends CompletionItemKind(folder)

  case object EnumMember extends CompletionItemKind(enumMember)

  case object Constant extends CompletionItemKind(constant)

  case object Struct extends CompletionItemKind(struct)

  case object Event extends CompletionItemKind(event)

  case object Operator extends CompletionItemKind(operator)

  case object TypeParameter extends CompletionItemKind(typeParameter)

  implicit val textDocumentSyncKindDecoder: Decoder[CompletionItemKind] =
    Decoder.decodeInt.emap {
      case `text`          => Right(Text)
      case `method`        => Right(Method)
      case `function`      => Right(Function)
      case `constructor`   => Right(Constructor)
      case `field`         => Right(Field)
      case `variable`      => Right(Variable)
      case `classKind`     => Right(Class)
      case `interface`     => Right(Interface)
      case `module`        => Right(Module)
      case `property`      => Right(Property)
      case `unit`          => Right(Unit)
      case `value`         => Right(Value)
      case `enum`          => Right(Enum)
      case `keyword`       => Right(Keyword)
      case `snippet`       => Right(Snippet)
      case `color`         => Right(Color)
      case `file`          => Right(File)
      case `reference`     => Right(Reference)
      case `folder`        => Right(Folder)
      case `enumMember`    => Right(EnumMember)
      case `constant`      => Right(Constant)
      case `struct`        => Right(Struct)
      case `event`         => Right(Event)
      case `operator`      => Right(Operator)
      case `typeParameter` => Right(TypeParameter)
      case _               => Left(invalidCompletionItemKind)
    }
}
