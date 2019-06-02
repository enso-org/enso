/** The tokens returned by the scanner. */

package org.enso.syntax.text.lexer

// class Token (index:Int, text:String, line:Int, charBegin:Int, charEnd:Int) {

//   override def toString() : String = {
//     return "Text   : " + text + "\nindex : " + index + "\nline  : " + line + "\ncBeg. : " + charBegin + "\ncEnd. : " + charEnd;
//   }
// }

// sealed trait Tree[+A]
// case object Leaf extends Node[Nothing]
// case class  Node[A](data: A, left: Tree[A], right: Tree[A]) extends Tree[A]

abstract class Bound
case object Begin extends Bound
case object End   extends Bound


///////////
// Token //
///////////

case class Token (symbol:Symbol, offset:Int, span:Int)


////////////
// Symbol //
////////////

abstract class Symbol 

// Identifiers
case class  Var  (name:String) extends Symbol
case class  Cons (name:String) extends Symbol
case object Wildcard           extends Symbol

// Operators
case class  Operator (name:String) extends Symbol
case class  Modifier (name:String) extends Symbol

// Literals
case object GroupBegin             extends Symbol
case object GroupEnd               extends Symbol
case object ListBegin              extends Symbol
case object ListEnd                extends Symbol
case object RecordBegin            extends Symbol
case object RecordEnd              extends Symbol


// Invalid
case class  Invalid (reason:InvalidReason) extends Symbol



abstract class InvalidReason
case class UnexpectedSuffix (text:String) extends InvalidReason

case class Unmatched (char:String) extends Symbol

// data Symbol
// // Layout
// case class STX
// case class ETX
// case class EOL
// case class Terminator
// case class BlockStart
// case class Block       !Bound
// case class Group       !Bound
// case class Marker      !Word64

//     -- Ident
//     | Var          !Text32
//     | Cons         !Text32
//     | Wildcard

//     -- Keyword
//     | KwCase
//     | KwClass
//     | KwDef
//     | KwForeign
//     | KwImport
//     | KwNative
//     | KwOf

//     -- Operator
//     | Operator    !Text32
//     | Modifier    !Text32
//     | Accessor

//     -- | Arrow
//     | Assignment
//     | Typed
//     | TypeApp
//     | Merge
//     | Range
//     | Anything

//     -- Literal
//     | Number      !Number
//     | Quote       !StrType  !Bound
//     | Str         !Text32
//     | StrEsc      !StrEscType
//     | List        !Bound

//     -- Comment
//     | Disable
//     | Doc         !Text32

//     -- Config
//     | Metadata    !Text32
//     -- | Pragma ...

//     -- Other
//     | Unknown     !Text32 -- DEPRECATED
//     | Incorrect   !Text32 -- DEPRECATED
//     | Invalid     !Invalid.Symbol
//     deriving (Eq, Generic, Ord, Show)

// data StrEscType
//     = CharStrEsc  !Int
//     | NumStrEsc   !Int
//     | QuoteEscape !StrType
//     | SlashEsc
//     deriving (Eq, Generic, Ord, Show)

// data Bound   = Begin | End              deriving (Eq, Generic, Ord, Show)
// data StrType = RawStr | FmtStr | NatStr deriving (Eq, Generic, Ord, Show)
// data Number  = NumRep
//     { _base     :: Word8
//     , _intPart  :: [Word8]
//     , _fracPart :: [Word8]
//     } deriving (Eq, Generic, Ord, Show)