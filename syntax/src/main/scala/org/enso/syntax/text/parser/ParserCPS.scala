package org.enso.syntax.text.parser

import java.io.Reader

//////////////////
// Parser Rules //
//////////////////

trait Result[Input, Output]
case class Fail[Input, Output](rest: Input,
                               contexts: List[String],
                               error: String)
    extends Result[Input, Output]
case class Partial[Input, Output](cont: Input => Result[Input, Output])
    extends Result[Input, Output]
case class Done[Input, Output](rest: Input, output: Output)
    extends Result[Input, Output]

case class Pos(offset: Int)

trait More
case object Complete   extends More
case object Incomplete extends More

class PP() {
  type Input  = Int
  type Output = Int
  type State  = Int

  type Failure =
    (State, Pos, More, List[String], String) => Result[Input, Output]
  type Success = (State, Pos, More, Output) => Result[Input, Output]

  //failK :: Failure a
  //failK t (Pos pos) _more stack msg = Fail (Buf.dropWord16 pos t) stack msg
  def failK(state: State,
            pos: Pos,
            more: More,
            stack: List[String],
            msg: String): Result[Input, Output] = Fail(0 /*!*/, stack, msg)

  //successK :: Success a a
  //successK t (Pos pos) _more a = Done (Buf.dropWord16 pos t) a
  def successK(state: State,
               pos: Pos,
               more: More,
               a: Input): Result[Input, Output] =
    Done(0 /*!*/, a)
//  ensure :: Int -> Parser (Pos, Text)
//  ensure n = T.Parser $ \t pos more lose succ ->
//  case lengthAtLeast pos n t of
//  Just n' -> succ t pos more (n', substring pos n' t)
//  -- The uncommon case is kept out-of-line to reduce code size:
//    Nothing -> ensureSuspended n t pos more lose succ

//  def ensure(n:Int): Parser2 =
//
//  class Parser2(
//    run: (State, Pos, More, Failure, Success) => Result[Input, Output]) {
//
//    //parse :: Parser a -> Text -> Result a
//    //parse m s = runParser m (buffer s) 0 Incomplete failK successK
//    def parse(input: Input): Result[Input, Output] =
//      this.run(input, Pos(0), Incomplete, failK, successK)
//  }
}
//
//-- | Run a parser.
//parse :: Parser a -> Text -> Result a
//parse m s = runParser m (buffer s) 0 Incomplete failK successK
//newtype Parser i a = Parser {
//runParser :: forall r.
//State i -> Pos -> More
//-> Failure i (State i)   r
//-> Success i (State i) a r
//-> IResult i r
//}
//
//type family State i
//type instance State ByteString = B.Buffer
//type instance State Text = T.Buffer
//
//type Failure i t   r = t -> Pos -> More -> [String] -> String
//-> IResult i r
//type Success i t a r = t -> Pos -> More -> a -> IResult i r
//
//-- | Have we read all available input?
//data More = Complete | Incomplete
//deriving (Eq, Show)
//newtype Parser i a = Parser {
//  runParser :: forall r.
//  State i -> Pos -> More
//  -> Failure i (State i)   r
//  -> Success i (State i) a r
//  -> IResult i r
//}
