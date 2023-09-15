import init, { parse_to_json } from '../../rust-ffi/pkg/rust_ffi'
import type { NonEmptyArray } from './array'
import type { Opt } from './opt'

const _wasm = await init()

export function parseEnso(code: string): Ast.Tree {
  const json = parse_to_json(code)
  return JSON.parse(json)
}

export namespace Ast {
  export interface Tree {
    span: Span
    variant: Variant
  }

  export type Token =
    | Token.AutoScope
    | Token.CloseSymbol
    | Token.Digits
    | Token.Ident
    | Token.Newline
    | Token.NumberBase
    | Token.OpenSymbol
    | Token.Operator
    | Token.TextEnd
    | Token.TextEscape
    | Token.TextSection
    | Token.TextStart
    | Token.Wildcard

  namespace Token {
    declare const Brand: unique symbol

    interface TokenBase<B, T = {}> {
      left_offset: Offset
      code: Code
      variant: T & { [Brand]: B }
    }

    export type AutoScope = TokenBase<'AutoScope'>
    export type CloseSymbol = TokenBase<'CloseSymbol'>
    export type Digits = TokenBase<'Digits', { base: Opt<'Binary' | 'Octal' | 'Hexadecimal'> }>
    export type Ident = TokenBase<
      'Ident',
      {
        is_free: boolean
        lift_level: number
        is_type: boolean
        is_operator_lexically: boolean
      }
    >
    export type Newline = TokenBase<'Newline'>
    export type NumberBase = TokenBase<'NumberBase'>
    export type OpenSymbol = TokenBase<'OpenSymbol'>
    export type Operator = TokenBase<'Operator'>
    export type TextEnd = TokenBase<'TextEnd'>
    export type TextEscape = TokenBase<
      'TextEscape',
      {
        /**
         * Escaped character Unicode scalar value, Serialized from Rust's `char`.
         * https://doc.rust-lang.org/std/primitive.char.html
         */
        value: Opt<number>
      }
    >
    export type TextSection = TokenBase<'TextSection'>
    export type TextStart = TokenBase<'TextStart'>
    export type Wildcard = TokenBase<'Wildcard', { lift_level: number }>
  }

  export interface Span {
    code_length: Length
    left_offset: Offset
  }

  export interface Code {
    repr: CowStrPtr
    utf16: number
  }

  export interface Length {
    utf8: number
    utf16: number
  }

  export interface Offset {
    visible: VisibleOffset
    code: Code
  }

  export interface CowStrPtr {
    begin: number
    len: number
  }

  export interface VisibleOffset {
    width_in_spaces: number
  }

  export type Variant =
    | { Invalid: Variant.Invalid }
    | { BodyBlock: Variant.BodyBlock }
    | { ArgumentBlockApplication: Variant.ArgumentBlockApplication }
    | { OperatorBlockApplication: Variant.OperatorBlockApplication }
    | { Ident: Variant.Ident }
    | { Number: Variant.Number }
    | { Wildcard: Variant.Wildcard }
    | { AutoScope: Variant.AutoScope }
    | { TextLiteral: Variant.TextLiteral }
    | { App: Variant.App }
    | { NamedApp: Variant.NamedApp }
    | { DefaultApp: Variant.DefaultApp }
    | { OprApp: Variant.OprApp }
    | { UnaryOprApp: Variant.UnaryOprApp }
    | { OprSectionBoundary: Variant.OprSectionBoundary }
    | { TemplateFunction: Variant.TemplateFunction }
    | { MultiSegmentApp: Variant.MultiSegmentApp }
    | { TypeDef: Variant.TypeDef }
    | { Assignment: Variant.Assignment }
    | { Function: Variant.Function }
    | { ForeignFunction: Variant.ForeignFunction }
    | { Import: Variant.Import }
    | { Export: Variant.Export }
    | { Group: Variant.Group }
    | { TypeSignature: Variant.TypeSignature }
    | { TypeAnnotated: Variant.TypeAnnotated }
    | { CaseOf: Variant.CaseOf }
    | { Lambda: Variant.Lambda }
    | { Array: Variant.Array }
    | { Tuple: Variant.Tuple }
    | { Annotated: Variant.Annotated }
    | { AnnotatedBuiltin: Variant.AnnotatedBuiltin }
    | { Documented: Variant.Documented }
    | { ConstructorDefinition: Variant.ConstructorDefinition }

  export namespace Variant {
    export interface Invalid {
      error: Error
      ast: Tree
    }

    export interface BlockLine {
      newline: Token.Newline
      expression: Opt<Tree>
    }

    export interface BlockOperatorLine {}

    export interface BodyBlock {
      statements: BlockLine[]
    }

    export interface ArgumentBlockApplication {
      lhs: Opt<Tree>
      arguments: BlockLine[]
    }

    export interface OperatorBlockApplication {
      lhs: Opt<Tree>
      expressions: BlockOperatorLine[]
      excess: BlockLine[]
    }
    export interface Ident {
      token: Token.Ident
    }

    export interface FractionalDigits {
      dot: Token.Operator
      digits: Token.Digits
    }

    export interface Number {
      base: Opt<Token.NumberBase>
      integer: Opt<Token.Digits>
      fractional_digits: Opt<FractionalDigits>
    }
    export interface Wildcard {
      token: Token.Wildcard
      de_bruijn_index: Opt<number>
    }

    export interface AutoScope {
      token: Token.AutoScope
    }

    export interface TextLiteral {
      open: Opt<Token.TextStart>
      newline: Opt<Token.Newline>
      elements: TextElement[]
      close: Opt<Token.TextEnd>
    }

    export interface App {
      func: Tree
      arg: Tree
    }

    export interface NamedApp {
      func: Tree
      open: Opt<Token.OpenSymbol>
      name: Token.Ident
      equals: Token.Operator
      arg: Tree
      close: Opt<Token.CloseSymbol>
    }

    export interface DefaultApp {
      func: Tree
      default: Token.Ident
    }

    type Result<T, E> = { Ok: T } | { Err: E }

    export interface OprApp {
      lhs: Opt<Tree>
      opr: Result<Token, Error>
      rhs: Opt<Tree>
    }

    export interface UnaryOprApp {
      opr: Token.Operator
      rhs: Opt<Tree>
    }

    export interface OprSectionBoundary {
      arguments: number
      ast: Tree
    }

    export interface TemplateFunction {
      arguments: number
      ast: Tree
    }

    export interface MultiSegmentApp {
      segments: NonEmptyArray<MultiSegmentAppSegment>
    }

    export interface MultiSegmentAppSegment {
      header: Token
      body: Opt<Tree>
    }

    export interface ArgumentType {
      operator: Token.Operator
      type: Tree
    }

    export interface ArgumentDefault {
      equals: Token.Operator
      expression: Tree
    }

    /** A function argument definition.  */
    export interface ArgumentDefinition {
      /** Opening parenthesis (outer). */
      open: Opt<Token.OpenSymbol>
      /** Opening parenthesis (inner). */
      open2: Opt<Token.OpenSymbol>
      /** An optional execution-suspension unary operator (~). */
      suspension: Opt<Token.Operator>
      /** The pattern being bound to an argument. */
      pattern: Tree
      /** An optional type ascribed to an argument. */
      type: Opt<ArgumentType>
      /** Closing parenthesis (inner). */
      close2: Opt<Token.CloseSymbol>
      /** An optional default value for an argument. */
      default: Opt<ArgumentDefault>
      /** Closing parenthesis (outer). */
      close: Opt<Token.CloseSymbol>
    }

    export interface ArgumentDefinitionLine {
      newline: Token.Newline
      argument: Opt<ArgumentDefinition>
    }

    export interface TypeDef {
      keyword: Token.Ident
      name: Token.Ident
      params: ArgumentDefinition[]
      body: BlockLine[]
    }

    export interface Assignment {
      pattern: Tree
      equals: Token.Operator
      expr: Tree
    }

    export interface Function {
      name: Tree
      args: ArgumentDefinition[]
      equals: Token.Operator
      body: Opt<Tree>
    }

    export interface ForeignFunction {
      foreign: Token.Ident
      language: Token.Ident
      name: Token.Ident
      args: ArgumentDefinition[]
      equals: Token.Operator
      body: Tree
    }

    export interface Import {
      polyglot: Opt<MultiSegmentAppSegment>
      from: Opt<MultiSegmentAppSegment>
      import: MultiSegmentAppSegment
      all: Opt<Token.Ident>
      as_: Opt<MultiSegmentAppSegment>
      hiding: Opt<MultiSegmentAppSegment>
    }

    export interface Export {
      from: Opt<MultiSegmentAppSegment>
      export: MultiSegmentAppSegment
      all: Opt<Token.Ident>
      as_: Opt<MultiSegmentAppSegment>
      hiding: Opt<MultiSegmentAppSegment>
    }

    export interface Group {
      open: Opt<Token.OpenSymbol>
      body: Opt<Tree>
      close: Opt<Token.CloseSymbol>
    }

    export interface TypeSignature {
      variable: Tree
      operator: Token.Operator
      type_: Tree
    }

    export interface TypeAnnotated {
      expression: Tree
      operator: Token.Operator
      type_: Tree
    }

    export interface CaseLine {
      newline: Opt<Token.Newline>
      case: Opt<Case>
    }

    export interface Case {
      documentation: Opt<DocComment>
      pattern: Opt<Tree>
      arrow: Opt<Token.Operator>
      expression: Opt<Tree>
    }

    export interface CaseOf {
      case: Token.Ident
      expression: Opt<Tree>
      of: Token.Ident
      cases: CaseLine[]
    }

    export interface Lambda {
      operator: Token.Operator
      arrow: Opt<Tree>
    }

    export interface OperatorDelimitedTree {
      operator: Token.Operator
      body: Opt<Tree>
    }

    export interface Array {
      left: Token.OpenSymbol
      first: Opt<Tree>
      rest: OperatorDelimitedTree[]
      right: Token.CloseSymbol
    }

    export interface Tuple {
      left: Token.OpenSymbol
      first: Opt<Tree>
      rest: OperatorDelimitedTree[]
      right: Token.CloseSymbol
    }

    export interface Annotated {
      token: Token.Operator
      annotation: Token.Ident
      argument: Opt<Tree>
      newlines: Token.Newline[]
      expression: Opt<Tree>
    }

    export interface AnnotatedBuiltin {
      token: Token.Operator
      annotation: Token.Ident
      newlines: Token.Newline[]
      expression: Opt<Tree>
    }

    export interface Documented {
      documentation: DocComment
      expression: Opt<Tree>
    }

    export interface ConstructorDefinition {
      constructor: Token.Ident
      arguments: ArgumentDefinition[]
      block: ArgumentDefinitionLine[]
    }
  }

  export interface DocComment {
    open: Token.TextStart
    elements: TextElement[]
    newlines: Token.Newline[]
  }

  export type TextElement =
    | { Section: TextElement.Section }
    | { Escape: TextElement.Escape }
    | { Newline: TextElement.Newline }
    | { Splice: TextElement.Splice }

  export namespace TextElement {
    export interface Section {
      text: Token.TextSection
    }

    export interface Escape {
      token: Token.TextEscape
    }

    export interface Newline {
      newline: Token.Newline
    }

    export interface Splice {
      open: Token.OpenSymbol
      expression: Opt<Tree>
      close: Token.CloseSymbol
    }
  }

  export interface Error {
    message: string
  }
}
