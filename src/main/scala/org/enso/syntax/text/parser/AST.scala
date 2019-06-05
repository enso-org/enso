package org.enso.syntax.text.parser


/////////
// AST //
/////////

case class AST (offset:Int, span:Int, symbol:Symbol) 



////////////
// Symbol //
////////////

abstract class Symbol 
case object NONE                           extends Symbol

// Identifiers
case class Var      (name:String)          extends Symbol
case class Operator (name:String)          extends Symbol
case class App      (func:AST, arg:AST)    extends Symbol
case class Block    (body:Vector[AST])     extends Symbol