export class LazyObject {
  protected readonly lazyObjectData: Cursor

  constructor(data: Cursor) {
    this.lazyObjectData = data
  }

  debug(): {} {
    return {}
  }
}

export const builtin = {
  Array: Array
} as const

export class Cursor {
  private readonly blob: Uint8Array
  private readonly address: number

  constructor(blob: Uint8Array, address: number) {
    this.blob = blob
    this.address = address
  }

  * readSequence<T>(
    readElement: (cursor: Cursor) => T,
    elementSize: number
  ): Iterable<T> {
    const data = this.readPointer()
    let count = data.readU32()
    let offset = 4
    while (count > 0) {
      yield readElement(data.seek(offset))
      count--
      offset += elementSize
    }
  }

  readOption<T>(
    readElement: (cursor: Cursor) => T
  ): T | null {
    const discriminant = this.readU8()
    switch (discriminant) {
      case 0:
        return null
      case 1:
        return readElement(this.seek(1).readPointer())
      default:
        throw new Error(`Invalid Option discriminant: 0x${discriminant.toString(16)}.`)
    }
  }

  readResult<Ok, Err>(
    readOk: (cursor: Cursor) => Ok,
    readErr: (cursor: Cursor) => Err
  ): Ok | Err {
    const data = this.readPointer()
    const discriminant = data.readU32()
    switch (discriminant) {
      case 0:
        return readOk(data.seek(4))
      case 1:
        return readErr(data.seek(4))
      default:
        throw new Error(`Invalid Result discriminant: 0x${discriminant.toString(16)}.`)
    }
  }

  readPointer(): Cursor {
    return new Cursor(this.blob, this.readU32())
  }

  readU8(): number {
    return this.blob.at(this.address)!
  }

  readU32(): number {
    return this.readU8()
      | (this.seek(1).readU8() << 8)
      | (this.seek(2).readU8() << 16)
      | (this.seek(3).readU8() << 24)
  }

  readI32(): number {
    const raw = this.readU32()
    const value = raw & 0x7fff_ffff
    if (value === raw) {
      return value
    } else {
      return -value
    }
  }

  readU64(): number {
    const lo = this.readU32()
    const hi = this.seek(4).readU32()
    //if (hi !== 0) {
    //  throw new RangeError()
    //}
    return lo
  }

  readBool(): boolean {
    const value = this.readU8()
    switch (value) {
    case 0:
      return false
    case 1:
      return true
    default:
      throw new Error(`Invalid boolean: 0x${value.toString(16)} @ 0x${this.address.toString(16)}.`)
    }
  }

  readString(): string {
    const data = this.readPointer()
    const len = data.readU32()
    const stringData = data.seek(4)
    const bytes = stringData.blob.slice(stringData.address, stringData.address + len)
    return new TextDecoder().decode(bytes)
  }

  seek(offset: number): Cursor {
    return new Cursor(this.blob, this.address + offset)
  }
}

export function debugHelper(value: any): object | null {
  if (value === null) {
    return null
  }
  if (typeof value["debug"] === "function") {
    return value.debug()
  }
  if (typeof value[Symbol.iterator] === "function") {
    return Array.from(value, debugHelper)
  }
  return value
}
export class DocComment extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): DocComment { return new DocComment(cursor); }
    get open(): Token.TextStart { return Token.TextStart.read(this.lazyObjectData); }
    get elements(): Iterable<TextElement> { return this.lazyObjectData.seek(40).readSequence((element: Cursor) => TextElement.read(element.readPointer()), 4); }
    get newlines(): Iterable<Token.Newline> { return this.lazyObjectData.seek(44).readSequence((element: Cursor) => Token.Newline.read(element), 40); }
    debug(): any { return { ...super.debug(), open: debugHelper(this.open), elements: debugHelper(this.elements), newlines: debugHelper(this.newlines) }; }
}
export module TextElement {
    abstract class AbstractBase extends LazyObject {
        constructor(cursor: Cursor) { super(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export const enum Type {
        Section = 0,
        Escape = 1,
        Newline = 2,
        Splice = 3
    }
    export class Section extends AbstractBase {
        readonly type: Type.Section;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Section; }
        static read(cursor: Cursor): Section { return new Section(cursor); }
        get text(): Token.TextSection { return Token.TextSection.read(this.lazyObjectData); }
        debug(): any { return { ...super.debug(), text: debugHelper(this.text) }; }
    }
    export class Escape extends AbstractBase {
        readonly type: Type.Escape;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Escape; }
        static read(cursor: Cursor): Escape { return new Escape(cursor); }
        get token(): Token.TextEscape { return Token.TextEscape.read(this.lazyObjectData); }
        debug(): any { return { ...super.debug(), token: debugHelper(this.token) }; }
    }
    export class Newline extends AbstractBase {
        readonly type: Type.Newline;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Newline; }
        static read(cursor: Cursor): Newline { return new Newline(cursor); }
        get newline(): Token.Newline { return Token.Newline.read(this.lazyObjectData); }
        debug(): any { return { ...super.debug(), newline: debugHelper(this.newline) }; }
    }
    export class Splice extends AbstractBase {
        readonly type: Type.Splice;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Splice; }
        static read(cursor: Cursor): Splice { return new Splice(cursor); }
        get open(): Token.OpenSymbol { return Token.OpenSymbol.read(this.lazyObjectData); }
        get expression(): Tree | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get close(): Token.CloseSymbol { return Token.CloseSymbol.read(this.lazyObjectData.seek(45)); }
        debug(): any { return { ...super.debug(), open: debugHelper(this.open), expression: debugHelper(this.expression), close: debugHelper(this.close) }; }
    }
    export type TextElement = Section | Escape | Newline | Splice;
    export function read(cursor: Cursor): TextElement { switch (cursor.readU32()) {
        case 0: return new Section(cursor.seek(4));
        case 1: return new Escape(cursor.seek(4));
        case 2: return new Newline(cursor.seek(4));
        case 3: return new Splice(cursor.seek(4));
        default: throw new Error("Unexpected discriminant while deserializing.");
    } }
}
export type TextElement = TextElement.TextElement;
export class OperatorDelimitedTree extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): OperatorDelimitedTree { return new OperatorDelimitedTree(cursor); }
    get operator(): Token.Operator { return Token.Operator.read(this.lazyObjectData); }
    get body(): Tree | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Tree.read(element.readPointer())); }
    debug(): any { return { ...super.debug(), operator: debugHelper(this.operator), body: debugHelper(this.body) }; }
}
export class ArgumentDefault extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): ArgumentDefault { return new ArgumentDefault(cursor); }
    get equals(): Token.Operator { return Token.Operator.read(this.lazyObjectData); }
    get expression(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
    debug(): any { return { ...super.debug(), equals: debugHelper(this.equals), expression: debugHelper(this.expression) }; }
}
export class ArgumentDefinition extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): ArgumentDefinition { return new ArgumentDefinition(cursor); }
    get open(): Token.OpenSymbol | null { return this.lazyObjectData.readOption((element: Cursor) => Token.OpenSymbol.read(element)); }
    get open2(): Token.OpenSymbol | null { return this.lazyObjectData.seek(5).readOption((element: Cursor) => Token.OpenSymbol.read(element)); }
    get suspension(): Token.Operator | null { return this.lazyObjectData.seek(10).readOption((element: Cursor) => Token.Operator.read(element)); }
    get pattern(): Tree { return Tree.read(this.lazyObjectData.seek(15).readPointer()); }
    get typeNode(): ArgumentType | null { return this.lazyObjectData.seek(19).readOption((element: Cursor) => ArgumentType.read(element)); }
    get close2(): Token.CloseSymbol | null { return this.lazyObjectData.seek(24).readOption((element: Cursor) => Token.CloseSymbol.read(element)); }
    get default(): ArgumentDefault | null { return this.lazyObjectData.seek(29).readOption((element: Cursor) => ArgumentDefault.read(element)); }
    get close(): Token.CloseSymbol | null { return this.lazyObjectData.seek(34).readOption((element: Cursor) => Token.CloseSymbol.read(element)); }
    debug(): any { return { ...super.debug(), open: debugHelper(this.open), open2: debugHelper(this.open2), suspension: debugHelper(this.suspension), pattern: debugHelper(this.pattern), typeNode: debugHelper(this.typeNode), close2: debugHelper(this.close2), default: debugHelper(this.default), close: debugHelper(this.close) }; }
}
export module Base {
    abstract class AbstractBase extends LazyObject {
        constructor(cursor: Cursor) { super(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export const enum Type {
        Binary = 0,
        Octal = 1,
        Hexadecimal = 2
    }
    export class Binary extends AbstractBase {
        readonly type: Type.Binary;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Binary; }
        static read(cursor: Cursor): Binary { return new Binary(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class Octal extends AbstractBase {
        readonly type: Type.Octal;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Octal; }
        static read(cursor: Cursor): Octal { return new Octal(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class Hexadecimal extends AbstractBase {
        readonly type: Type.Hexadecimal;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Hexadecimal; }
        static read(cursor: Cursor): Hexadecimal { return new Hexadecimal(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export type Base = Binary | Octal | Hexadecimal;
    export function read(cursor: Cursor): Base { switch (cursor.readU32()) {
        case 0: return new Binary(cursor.seek(4));
        case 1: return new Octal(cursor.seek(4));
        case 2: return new Hexadecimal(cursor.seek(4));
        default: throw new Error("Unexpected discriminant while deserializing.");
    } }
}
export type Base = Base.Base;
export class OperatorLine extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): OperatorLine { return new OperatorLine(cursor); }
    get newline(): Token.Newline { return Token.Newline.read(this.lazyObjectData); }
    get expression(): OperatorBlockExpression | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => OperatorBlockExpression.read(element)); }
    debug(): any { return { ...super.debug(), newline: debugHelper(this.newline), expression: debugHelper(this.expression) }; }
}
export class OperatorBlockExpression extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): OperatorBlockExpression { return new OperatorBlockExpression(cursor); }
    get operator(): Token.Operator | MultipleOperatorError { return this.lazyObjectData.readResult((okData: Cursor) => Token.Operator.read(okData), (errData: Cursor) => MultipleOperatorError.read(errData)); }
    get expression(): Tree { return Tree.read(this.lazyObjectData.seek(4).readPointer()); }
    debug(): any { return { ...super.debug(), operator: debugHelper(this.operator), expression: debugHelper(this.expression) }; }
}
export class ArgumentDefinitionLine extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): ArgumentDefinitionLine { return new ArgumentDefinitionLine(cursor); }
    get newline(): Token.Newline { return Token.Newline.read(this.lazyObjectData); }
    get argument(): ArgumentDefinition | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => ArgumentDefinition.read(element)); }
    debug(): any { return { ...super.debug(), newline: debugHelper(this.newline), argument: debugHelper(this.argument) }; }
}
export class Case extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): Case { return new Case(cursor); }
    get documentation(): DocComment | null { return this.lazyObjectData.readOption((element: Cursor) => DocComment.read(element)); }
    get pattern(): Tree | null { return this.lazyObjectData.seek(5).readOption((element: Cursor) => Tree.read(element.readPointer())); }
    get arrow(): Token.Operator | null { return this.lazyObjectData.seek(10).readOption((element: Cursor) => Token.Operator.read(element)); }
    get expression(): Tree | null { return this.lazyObjectData.seek(15).readOption((element: Cursor) => Tree.read(element.readPointer())); }
    debug(): any { return { ...super.debug(), documentation: debugHelper(this.documentation), pattern: debugHelper(this.pattern), arrow: debugHelper(this.arrow), expression: debugHelper(this.expression) }; }
}
export module Token {
    abstract class AbstractBase extends LazyObject {
        constructor(cursor: Cursor) { super(cursor); }
        get leftOffsetVisible(): number { return this.lazyObjectData.readU64(); }
        get leftOffsetCodeReprBegin(): number { return this.lazyObjectData.seek(8).readU32(); }
        get leftOffsetCodeReprLen(): number { return this.lazyObjectData.seek(12).readU32(); }
        get leftOffsetCodeUtf16(): number { return this.lazyObjectData.seek(16).readU64(); }
        get codeReprBegin(): number { return this.lazyObjectData.seek(24).readU32(); }
        get codeReprLen(): number { return this.lazyObjectData.seek(28).readU32(); }
        get codeUtf16(): number { return this.lazyObjectData.seek(32).readU64(); }
        debug(): any { return { ...super.debug(), leftOffsetVisible: debugHelper(this.leftOffsetVisible), leftOffsetCodeReprBegin: debugHelper(this.leftOffsetCodeReprBegin), leftOffsetCodeReprLen: debugHelper(this.leftOffsetCodeReprLen), leftOffsetCodeUtf16: debugHelper(this.leftOffsetCodeUtf16), codeReprBegin: debugHelper(this.codeReprBegin), codeReprLen: debugHelper(this.codeReprLen), codeUtf16: debugHelper(this.codeUtf16) }; }
    }
    export const enum Type {
        Newline = 0,
        OpenSymbol = 1,
        CloseSymbol = 2,
        BlockStart = 3,
        BlockEnd = 4,
        Wildcard = 5,
        AutoScope = 6,
        Ident = 7,
        Operator = 8,
        Digits = 9,
        NumberBase = 10,
        TextStart = 11,
        TextEnd = 12,
        TextSection = 13,
        TextEscape = 14,
        TextInitialNewline = 15,
        TextNewline = 16,
        Invalid = 17
    }
    export class Newline extends AbstractBase {
        readonly type: Type.Newline;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Newline; }
        static read(cursor: Cursor): Newline { return new Newline(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class OpenSymbol extends AbstractBase {
        readonly type: Type.OpenSymbol;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.OpenSymbol; }
        static read(cursor: Cursor): OpenSymbol { return new OpenSymbol(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class CloseSymbol extends AbstractBase {
        readonly type: Type.CloseSymbol;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.CloseSymbol; }
        static read(cursor: Cursor): CloseSymbol { return new CloseSymbol(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class BlockStart extends AbstractBase {
        readonly type: Type.BlockStart;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.BlockStart; }
        static read(cursor: Cursor): BlockStart { return new BlockStart(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class BlockEnd extends AbstractBase {
        readonly type: Type.BlockEnd;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.BlockEnd; }
        static read(cursor: Cursor): BlockEnd { return new BlockEnd(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class Wildcard extends AbstractBase {
        readonly type: Type.Wildcard;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Wildcard; }
        static read(cursor: Cursor): Wildcard { return new Wildcard(cursor); }
        get liftLevel(): number { return this.lazyObjectData.seek(40).readU64(); }
        debug(): any { return { ...super.debug(), liftLevel: debugHelper(this.liftLevel) }; }
    }
    export class AutoScope extends AbstractBase {
        readonly type: Type.AutoScope;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.AutoScope; }
        static read(cursor: Cursor): AutoScope { return new AutoScope(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class Ident extends AbstractBase {
        readonly type: Type.Ident;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Ident; }
        static read(cursor: Cursor): Ident { return new Ident(cursor); }
        get isFree(): boolean { return this.lazyObjectData.seek(40).readBool(); }
        get liftLevel(): number { return this.lazyObjectData.seek(41).readU64(); }
        get isTypeOrConstructor(): boolean { return this.lazyObjectData.seek(49).readBool(); }
        get isOperatorLexically(): boolean { return this.lazyObjectData.seek(50).readBool(); }
        debug(): any { return { ...super.debug(), isFree: debugHelper(this.isFree), liftLevel: debugHelper(this.liftLevel), isTypeOrConstructor: debugHelper(this.isTypeOrConstructor), isOperatorLexically: debugHelper(this.isOperatorLexically) }; }
    }
    export class Operator extends AbstractBase {
        readonly type: Type.Operator;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Operator; }
        static read(cursor: Cursor): Operator { return new Operator(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class Digits extends AbstractBase {
        readonly type: Type.Digits;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Digits; }
        static read(cursor: Cursor): Digits { return new Digits(cursor); }
        get base(): Base | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Base.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), base: debugHelper(this.base) }; }
    }
    export class NumberBase extends AbstractBase {
        readonly type: Type.NumberBase;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.NumberBase; }
        static read(cursor: Cursor): NumberBase { return new NumberBase(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class TextStart extends AbstractBase {
        readonly type: Type.TextStart;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextStart; }
        static read(cursor: Cursor): TextStart { return new TextStart(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class TextEnd extends AbstractBase {
        readonly type: Type.TextEnd;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextEnd; }
        static read(cursor: Cursor): TextEnd { return new TextEnd(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class TextSection extends AbstractBase {
        readonly type: Type.TextSection;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextSection; }
        static read(cursor: Cursor): TextSection { return new TextSection(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class TextEscape extends AbstractBase {
        readonly type: Type.TextEscape;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextEscape; }
        static read(cursor: Cursor): TextEscape { return new TextEscape(cursor); }
        get value(): number { return this.lazyObjectData.seek(40).readU32(); }
        debug(): any { return { ...super.debug(), value: debugHelper(this.value) }; }
    }
    export class TextInitialNewline extends AbstractBase {
        readonly type: Type.TextInitialNewline;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextInitialNewline; }
        static read(cursor: Cursor): TextInitialNewline { return new TextInitialNewline(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class TextNewline extends AbstractBase {
        readonly type: Type.TextNewline;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextNewline; }
        static read(cursor: Cursor): TextNewline { return new TextNewline(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export class Invalid extends AbstractBase {
        readonly type: Type.Invalid;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Invalid; }
        static read(cursor: Cursor): Invalid { return new Invalid(cursor); }
        debug(): any { return { ...super.debug() }; }
    }
    export type Token = Newline | OpenSymbol | CloseSymbol | BlockStart | BlockEnd | Wildcard | AutoScope | Ident | Operator | Digits | NumberBase | TextStart | TextEnd | TextSection | TextEscape | TextInitialNewline | TextNewline | Invalid;
    export function read(cursor: Cursor): Token { switch (cursor.readU32()) {
        case 0: return new Newline(cursor.seek(4));
        case 1: return new OpenSymbol(cursor.seek(4));
        case 2: return new CloseSymbol(cursor.seek(4));
        case 3: return new BlockStart(cursor.seek(4));
        case 4: return new BlockEnd(cursor.seek(4));
        case 5: return new Wildcard(cursor.seek(4));
        case 6: return new AutoScope(cursor.seek(4));
        case 7: return new Ident(cursor.seek(4));
        case 8: return new Operator(cursor.seek(4));
        case 9: return new Digits(cursor.seek(4));
        case 10: return new NumberBase(cursor.seek(4));
        case 11: return new TextStart(cursor.seek(4));
        case 12: return new TextEnd(cursor.seek(4));
        case 13: return new TextSection(cursor.seek(4));
        case 14: return new TextEscape(cursor.seek(4));
        case 15: return new TextInitialNewline(cursor.seek(4));
        case 16: return new TextNewline(cursor.seek(4));
        case 17: return new Invalid(cursor.seek(4));
        default: throw new Error("Unexpected discriminant while deserializing.");
    } }
}
export type Token = Token.Token;
export class CaseLine extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): CaseLine { return new CaseLine(cursor); }
    get newline(): Token.Newline | null { return this.lazyObjectData.readOption((element: Cursor) => Token.Newline.read(element)); }
    get case(): Case | null { return this.lazyObjectData.seek(5).readOption((element: Cursor) => Case.read(element)); }
    debug(): any { return { ...super.debug(), newline: debugHelper(this.newline), case: debugHelper(this.case) }; }
}
export class MultipleOperatorError extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): MultipleOperatorError { return new MultipleOperatorError(cursor); }
    get operators(): Iterable<Token.Operator> { return this.lazyObjectData.readSequence((element: Cursor) => Token.Operator.read(element), 40); }
    debug(): any { return { ...super.debug(), operators: debugHelper(this.operators) }; }
}
export module Tree {
    abstract class AbstractBase extends LazyObject {
        constructor(cursor: Cursor) { super(cursor); }
        get spanLeftOffsetVisible(): number { return this.lazyObjectData.readU64(); }
        get spanLeftOffsetCodeReprBegin(): number { return this.lazyObjectData.seek(8).readU32(); }
        get spanLeftOffsetCodeReprLen(): number { return this.lazyObjectData.seek(12).readU32(); }
        get spanLeftOffsetCodeUtf16(): number { return this.lazyObjectData.seek(16).readU64(); }
        get spanCodeLengthUtf8(): number { return this.lazyObjectData.seek(24).readU64(); }
        get spanCodeLengthUtf16(): number { return this.lazyObjectData.seek(32).readU64(); }
        debug(): any { return { ...super.debug(), spanLeftOffsetVisible: debugHelper(this.spanLeftOffsetVisible), spanLeftOffsetCodeReprBegin: debugHelper(this.spanLeftOffsetCodeReprBegin), spanLeftOffsetCodeReprLen: debugHelper(this.spanLeftOffsetCodeReprLen), spanLeftOffsetCodeUtf16: debugHelper(this.spanLeftOffsetCodeUtf16), spanCodeLengthUtf8: debugHelper(this.spanCodeLengthUtf8), spanCodeLengthUtf16: debugHelper(this.spanCodeLengthUtf16) }; }
    }
    export const enum Type {
        Invalid = 0,
        BodyBlock = 1,
        ArgumentBlockApplication = 2,
        OperatorBlockApplication = 3,
        Ident = 4,
        Number = 5,
        Wildcard = 6,
        AutoScope = 7,
        TextLiteral = 8,
        App = 9,
        NamedApp = 10,
        DefaultApp = 11,
        OprApp = 12,
        UnaryOprApp = 13,
        OprSectionBoundary = 14,
        TemplateFunction = 15,
        MultiSegmentApp = 16,
        TypeDef = 17,
        Assignment = 18,
        Function = 19,
        ForeignFunction = 20,
        Import = 21,
        Export = 22,
        Group = 23,
        TypeSignature = 24,
        TypeAnnotated = 25,
        CaseOf = 26,
        Lambda = 27,
        Array = 28,
        Tuple = 29,
        Annotated = 30,
        AnnotatedBuiltin = 31,
        Documented = 32,
        ConstructorDefinition = 33
    }
    export class Invalid extends AbstractBase {
        readonly type: Type.Invalid;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Invalid; }
        static read(cursor: Cursor): Invalid { return new Invalid(cursor); }
        get error(): string { return this.lazyObjectData.seek(40).readString(); }
        get ast(): Tree { return Tree.read(this.lazyObjectData.seek(44).readPointer()); }
        debug(): any { return { ...super.debug(), error: debugHelper(this.error), ast: debugHelper(this.ast) }; }
    }
    export class BodyBlock extends AbstractBase {
        readonly type: Type.BodyBlock;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.BodyBlock; }
        static read(cursor: Cursor): BodyBlock { return new BodyBlock(cursor); }
        get statements(): Iterable<Line> { return this.lazyObjectData.seek(40).readSequence((element: Cursor) => Line.read(element), 45); }
        debug(): any { return { ...super.debug(), statements: debugHelper(this.statements) }; }
    }
    export class ArgumentBlockApplication extends AbstractBase {
        readonly type: Type.ArgumentBlockApplication;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.ArgumentBlockApplication; }
        static read(cursor: Cursor): ArgumentBlockApplication { return new ArgumentBlockApplication(cursor); }
        get lhs(): Tree | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get arguments(): Iterable<Line> { return this.lazyObjectData.seek(45).readSequence((element: Cursor) => Line.read(element), 45); }
        debug(): any { return { ...super.debug(), lhs: debugHelper(this.lhs), arguments: debugHelper(this.arguments) }; }
    }
    export class OperatorBlockApplication extends AbstractBase {
        readonly type: Type.OperatorBlockApplication;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.OperatorBlockApplication; }
        static read(cursor: Cursor): OperatorBlockApplication { return new OperatorBlockApplication(cursor); }
        get lhs(): Tree | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get expressions(): Iterable<OperatorLine> { return this.lazyObjectData.seek(45).readSequence((element: Cursor) => OperatorLine.read(element), 45); }
        get excess(): Iterable<Line> { return this.lazyObjectData.seek(49).readSequence((element: Cursor) => Line.read(element), 45); }
        debug(): any { return { ...super.debug(), lhs: debugHelper(this.lhs), expressions: debugHelper(this.expressions), excess: debugHelper(this.excess) }; }
    }
    export class Ident extends AbstractBase {
        readonly type: Type.Ident;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Ident; }
        static read(cursor: Cursor): Ident { return new Ident(cursor); }
        get token(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(40)); }
        debug(): any { return { ...super.debug(), token: debugHelper(this.token) }; }
    }
    export class Number extends AbstractBase {
        readonly type: Type.Number;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Number; }
        static read(cursor: Cursor): Number { return new Number(cursor); }
        get base(): Token.NumberBase | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Token.NumberBase.read(element)); }
        get integer(): Token.Digits | null { return this.lazyObjectData.seek(45).readOption((element: Cursor) => Token.Digits.read(element)); }
        get fractionalDigits(): FractionalDigits | null { return this.lazyObjectData.seek(50).readOption((element: Cursor) => FractionalDigits.read(element)); }
        debug(): any { return { ...super.debug(), base: debugHelper(this.base), integer: debugHelper(this.integer), fractionalDigits: debugHelper(this.fractionalDigits) }; }
    }
    export class Wildcard extends AbstractBase {
        readonly type: Type.Wildcard;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Wildcard; }
        static read(cursor: Cursor): Wildcard { return new Wildcard(cursor); }
        get token(): Token.Wildcard { return Token.Wildcard.read(this.lazyObjectData.seek(40)); }
        get deBruijnIndex(): number { return this.lazyObjectData.seek(88).readI32(); }
        debug(): any { return { ...super.debug(), token: debugHelper(this.token), deBruijnIndex: debugHelper(this.deBruijnIndex) }; }
    }
    export class AutoScope extends AbstractBase {
        readonly type: Type.AutoScope;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.AutoScope; }
        static read(cursor: Cursor): AutoScope { return new AutoScope(cursor); }
        get token(): Token.AutoScope { return Token.AutoScope.read(this.lazyObjectData.seek(40)); }
        debug(): any { return { ...super.debug(), token: debugHelper(this.token) }; }
    }
    export class TextLiteral extends AbstractBase {
        readonly type: Type.TextLiteral;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TextLiteral; }
        static read(cursor: Cursor): TextLiteral { return new TextLiteral(cursor); }
        get open(): Token.TextStart | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Token.TextStart.read(element)); }
        get newline(): Token.Newline | null { return this.lazyObjectData.seek(45).readOption((element: Cursor) => Token.Newline.read(element)); }
        get elements(): Iterable<TextElement> { return this.lazyObjectData.seek(50).readSequence((element: Cursor) => TextElement.read(element.readPointer()), 4); }
        get close(): Token.TextEnd | null { return this.lazyObjectData.seek(54).readOption((element: Cursor) => Token.TextEnd.read(element)); }
        debug(): any { return { ...super.debug(), open: debugHelper(this.open), newline: debugHelper(this.newline), elements: debugHelper(this.elements), close: debugHelper(this.close) }; }
    }
    export class App extends AbstractBase {
        readonly type: Type.App;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.App; }
        static read(cursor: Cursor): App { return new App(cursor); }
        get func(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get arg(): Tree { return Tree.read(this.lazyObjectData.seek(44).readPointer()); }
        debug(): any { return { ...super.debug(), func: debugHelper(this.func), arg: debugHelper(this.arg) }; }
    }
    export class NamedApp extends AbstractBase {
        readonly type: Type.NamedApp;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.NamedApp; }
        static read(cursor: Cursor): NamedApp { return new NamedApp(cursor); }
        get func(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get open(): Token.OpenSymbol | null { return this.lazyObjectData.seek(44).readOption((element: Cursor) => Token.OpenSymbol.read(element)); }
        get name(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(49)); }
        get equals(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(100)); }
        get arg(): Tree { return Tree.read(this.lazyObjectData.seek(140).readPointer()); }
        get close(): Token.CloseSymbol | null { return this.lazyObjectData.seek(144).readOption((element: Cursor) => Token.CloseSymbol.read(element)); }
        debug(): any { return { ...super.debug(), func: debugHelper(this.func), open: debugHelper(this.open), name: debugHelper(this.name), equals: debugHelper(this.equals), arg: debugHelper(this.arg), close: debugHelper(this.close) }; }
    }
    export class DefaultApp extends AbstractBase {
        readonly type: Type.DefaultApp;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.DefaultApp; }
        static read(cursor: Cursor): DefaultApp { return new DefaultApp(cursor); }
        get func(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get default(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(44)); }
        debug(): any { return { ...super.debug(), func: debugHelper(this.func), default: debugHelper(this.default) }; }
    }
    export class OprApp extends AbstractBase {
        readonly type: Type.OprApp;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.OprApp; }
        static read(cursor: Cursor): OprApp { return new OprApp(cursor); }
        get lhs(): Tree | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get opr(): Token.Operator | MultipleOperatorError { return this.lazyObjectData.seek(45).readResult((okData: Cursor) => Token.Operator.read(okData), (errData: Cursor) => MultipleOperatorError.read(errData)); }
        get rhs(): Tree | null { return this.lazyObjectData.seek(49).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), lhs: debugHelper(this.lhs), opr: debugHelper(this.opr), rhs: debugHelper(this.rhs) }; }
    }
    export class UnaryOprApp extends AbstractBase {
        readonly type: Type.UnaryOprApp;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.UnaryOprApp; }
        static read(cursor: Cursor): UnaryOprApp { return new UnaryOprApp(cursor); }
        get opr(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(40)); }
        get rhs(): Tree | null { return this.lazyObjectData.seek(80).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), opr: debugHelper(this.opr), rhs: debugHelper(this.rhs) }; }
    }
    export class OprSectionBoundary extends AbstractBase {
        readonly type: Type.OprSectionBoundary;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.OprSectionBoundary; }
        static read(cursor: Cursor): OprSectionBoundary { return new OprSectionBoundary(cursor); }
        get arguments(): number { return this.lazyObjectData.seek(40).readU32(); }
        get ast(): Tree { return Tree.read(this.lazyObjectData.seek(44).readPointer()); }
        debug(): any { return { ...super.debug(), arguments: debugHelper(this.arguments), ast: debugHelper(this.ast) }; }
    }
    export class TemplateFunction extends AbstractBase {
        readonly type: Type.TemplateFunction;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TemplateFunction; }
        static read(cursor: Cursor): TemplateFunction { return new TemplateFunction(cursor); }
        get arguments(): number { return this.lazyObjectData.seek(40).readU32(); }
        get ast(): Tree { return Tree.read(this.lazyObjectData.seek(44).readPointer()); }
        debug(): any { return { ...super.debug(), arguments: debugHelper(this.arguments), ast: debugHelper(this.ast) }; }
    }
    export class MultiSegmentApp extends AbstractBase {
        readonly type: Type.MultiSegmentApp;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.MultiSegmentApp; }
        static read(cursor: Cursor): MultiSegmentApp { return new MultiSegmentApp(cursor); }
        get segments(): Iterable<MultiSegmentAppSegment> { return this.lazyObjectData.seek(40).readSequence((element: Cursor) => MultiSegmentAppSegment.read(element), 9); }
        debug(): any { return { ...super.debug(), segments: debugHelper(this.segments) }; }
    }
    export class TypeDef extends AbstractBase {
        readonly type: Type.TypeDef;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TypeDef; }
        static read(cursor: Cursor): TypeDef { return new TypeDef(cursor); }
        get keyword(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(40)); }
        get name(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(91)); }
        get params(): Iterable<ArgumentDefinition> { return this.lazyObjectData.seek(142).readSequence((element: Cursor) => ArgumentDefinition.read(element), 39); }
        get body(): Iterable<Line> { return this.lazyObjectData.seek(146).readSequence((element: Cursor) => Line.read(element), 45); }
        debug(): any { return { ...super.debug(), keyword: debugHelper(this.keyword), name: debugHelper(this.name), params: debugHelper(this.params), body: debugHelper(this.body) }; }
    }
    export class Assignment extends AbstractBase {
        readonly type: Type.Assignment;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Assignment; }
        static read(cursor: Cursor): Assignment { return new Assignment(cursor); }
        get pattern(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get equals(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(44)); }
        get expr(): Tree { return Tree.read(this.lazyObjectData.seek(84).readPointer()); }
        debug(): any { return { ...super.debug(), pattern: debugHelper(this.pattern), equals: debugHelper(this.equals), expr: debugHelper(this.expr) }; }
    }
    export class Function extends AbstractBase {
        readonly type: Type.Function;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Function; }
        static read(cursor: Cursor): Function { return new Function(cursor); }
        get name(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get args(): Iterable<ArgumentDefinition> { return this.lazyObjectData.seek(44).readSequence((element: Cursor) => ArgumentDefinition.read(element), 39); }
        get equals(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(48)); }
        get body(): Tree | null { return this.lazyObjectData.seek(88).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), name: debugHelper(this.name), args: debugHelper(this.args), equals: debugHelper(this.equals), body: debugHelper(this.body) }; }
    }
    export class ForeignFunction extends AbstractBase {
        readonly type: Type.ForeignFunction;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.ForeignFunction; }
        static read(cursor: Cursor): ForeignFunction { return new ForeignFunction(cursor); }
        get foreign(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(40)); }
        get language(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(91)); }
        get name(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(142)); }
        get args(): Iterable<ArgumentDefinition> { return this.lazyObjectData.seek(193).readSequence((element: Cursor) => ArgumentDefinition.read(element), 39); }
        get equals(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(197)); }
        get body(): Tree { return Tree.read(this.lazyObjectData.seek(237).readPointer()); }
        debug(): any { return { ...super.debug(), foreign: debugHelper(this.foreign), language: debugHelper(this.language), name: debugHelper(this.name), args: debugHelper(this.args), equals: debugHelper(this.equals), body: debugHelper(this.body) }; }
    }
    export class Import extends AbstractBase {
        readonly type: Type.Import;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Import; }
        static read(cursor: Cursor): Import { return new Import(cursor); }
        get polyglot(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        get from(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(45).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        get import(): MultiSegmentAppSegment { return MultiSegmentAppSegment.read(this.lazyObjectData.seek(50)); }
        get all(): Token.Ident | null { return this.lazyObjectData.seek(59).readOption((element: Cursor) => Token.Ident.read(element)); }
        get as(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(64).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        get hiding(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(69).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        debug(): any { return { ...super.debug(), polyglot: debugHelper(this.polyglot), from: debugHelper(this.from), import: debugHelper(this.import), all: debugHelper(this.all), as: debugHelper(this.as), hiding: debugHelper(this.hiding) }; }
    }
    export class Export extends AbstractBase {
        readonly type: Type.Export;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Export; }
        static read(cursor: Cursor): Export { return new Export(cursor); }
        get from(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        get export(): MultiSegmentAppSegment { return MultiSegmentAppSegment.read(this.lazyObjectData.seek(45)); }
        get all(): Token.Ident | null { return this.lazyObjectData.seek(54).readOption((element: Cursor) => Token.Ident.read(element)); }
        get as(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(59).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        get hiding(): MultiSegmentAppSegment | null { return this.lazyObjectData.seek(64).readOption((element: Cursor) => MultiSegmentAppSegment.read(element)); }
        debug(): any { return { ...super.debug(), from: debugHelper(this.from), export: debugHelper(this.export), all: debugHelper(this.all), as: debugHelper(this.as), hiding: debugHelper(this.hiding) }; }
    }
    export class Group extends AbstractBase {
        readonly type: Type.Group;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Group; }
        static read(cursor: Cursor): Group { return new Group(cursor); }
        get open(): Token.OpenSymbol | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Token.OpenSymbol.read(element)); }
        get body(): Tree | null { return this.lazyObjectData.seek(45).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get close(): Token.CloseSymbol | null { return this.lazyObjectData.seek(50).readOption((element: Cursor) => Token.CloseSymbol.read(element)); }
        debug(): any { return { ...super.debug(), open: debugHelper(this.open), body: debugHelper(this.body), close: debugHelper(this.close) }; }
    }
    export class TypeSignature extends AbstractBase {
        readonly type: Type.TypeSignature;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TypeSignature; }
        static read(cursor: Cursor): TypeSignature { return new TypeSignature(cursor); }
        get variable(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get operator(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(44)); }
        get typeNode(): Tree { return Tree.read(this.lazyObjectData.seek(84).readPointer()); }
        debug(): any { return { ...super.debug(), variable: debugHelper(this.variable), operator: debugHelper(this.operator), typeNode: debugHelper(this.typeNode) }; }
    }
    export class TypeAnnotated extends AbstractBase {
        readonly type: Type.TypeAnnotated;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.TypeAnnotated; }
        static read(cursor: Cursor): TypeAnnotated { return new TypeAnnotated(cursor); }
        get expression(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
        get operator(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(44)); }
        get typeNode(): Tree { return Tree.read(this.lazyObjectData.seek(84).readPointer()); }
        debug(): any { return { ...super.debug(), expression: debugHelper(this.expression), operator: debugHelper(this.operator), typeNode: debugHelper(this.typeNode) }; }
    }
    export class CaseOf extends AbstractBase {
        readonly type: Type.CaseOf;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.CaseOf; }
        static read(cursor: Cursor): CaseOf { return new CaseOf(cursor); }
        get case(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(40)); }
        get expression(): Tree | null { return this.lazyObjectData.seek(91).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get of(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(96)); }
        get cases(): Iterable<CaseLine> { return this.lazyObjectData.seek(147).readSequence((element: Cursor) => CaseLine.read(element), 10); }
        debug(): any { return { ...super.debug(), case: debugHelper(this.case), expression: debugHelper(this.expression), of: debugHelper(this.of), cases: debugHelper(this.cases) }; }
    }
    export class Lambda extends AbstractBase {
        readonly type: Type.Lambda;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Lambda; }
        static read(cursor: Cursor): Lambda { return new Lambda(cursor); }
        get operator(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(40)); }
        get arrow(): Tree | null { return this.lazyObjectData.seek(80).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), operator: debugHelper(this.operator), arrow: debugHelper(this.arrow) }; }
    }
    export class Array extends AbstractBase {
        readonly type: Type.Array;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Array; }
        static read(cursor: Cursor): Array { return new Array(cursor); }
        get left(): Token.OpenSymbol { return Token.OpenSymbol.read(this.lazyObjectData.seek(40)); }
        get first(): Tree | null { return this.lazyObjectData.seek(80).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get rest(): Iterable<OperatorDelimitedTree> { return this.lazyObjectData.seek(85).readSequence((element: Cursor) => OperatorDelimitedTree.read(element), 45); }
        get right(): Token.CloseSymbol { return Token.CloseSymbol.read(this.lazyObjectData.seek(89)); }
        debug(): any { return { ...super.debug(), left: debugHelper(this.left), first: debugHelper(this.first), rest: debugHelper(this.rest), right: debugHelper(this.right) }; }
    }
    export class Tuple extends AbstractBase {
        readonly type: Type.Tuple;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Tuple; }
        static read(cursor: Cursor): Tuple { return new Tuple(cursor); }
        get left(): Token.OpenSymbol { return Token.OpenSymbol.read(this.lazyObjectData.seek(40)); }
        get first(): Tree | null { return this.lazyObjectData.seek(80).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get rest(): Iterable<OperatorDelimitedTree> { return this.lazyObjectData.seek(85).readSequence((element: Cursor) => OperatorDelimitedTree.read(element), 45); }
        get right(): Token.CloseSymbol { return Token.CloseSymbol.read(this.lazyObjectData.seek(89)); }
        debug(): any { return { ...super.debug(), left: debugHelper(this.left), first: debugHelper(this.first), rest: debugHelper(this.rest), right: debugHelper(this.right) }; }
    }
    export class Annotated extends AbstractBase {
        readonly type: Type.Annotated;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Annotated; }
        static read(cursor: Cursor): Annotated { return new Annotated(cursor); }
        get token(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(40)); }
        get annotation(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(80)); }
        get argument(): Tree | null { return this.lazyObjectData.seek(131).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        get newlines(): Iterable<Token.Newline> { return this.lazyObjectData.seek(136).readSequence((element: Cursor) => Token.Newline.read(element), 40); }
        get expression(): Tree | null { return this.lazyObjectData.seek(140).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), token: debugHelper(this.token), annotation: debugHelper(this.annotation), argument: debugHelper(this.argument), newlines: debugHelper(this.newlines), expression: debugHelper(this.expression) }; }
    }
    export class AnnotatedBuiltin extends AbstractBase {
        readonly type: Type.AnnotatedBuiltin;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.AnnotatedBuiltin; }
        static read(cursor: Cursor): AnnotatedBuiltin { return new AnnotatedBuiltin(cursor); }
        get token(): Token.Operator { return Token.Operator.read(this.lazyObjectData.seek(40)); }
        get annotation(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(80)); }
        get newlines(): Iterable<Token.Newline> { return this.lazyObjectData.seek(131).readSequence((element: Cursor) => Token.Newline.read(element), 40); }
        get expression(): Tree | null { return this.lazyObjectData.seek(135).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), token: debugHelper(this.token), annotation: debugHelper(this.annotation), newlines: debugHelper(this.newlines), expression: debugHelper(this.expression) }; }
    }
    export class Documented extends AbstractBase {
        readonly type: Type.Documented;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.Documented; }
        static read(cursor: Cursor): Documented { return new Documented(cursor); }
        get documentation(): DocComment { return DocComment.read(this.lazyObjectData.seek(40)); }
        get expression(): Tree | null { return this.lazyObjectData.seek(88).readOption((element: Cursor) => Tree.read(element.readPointer())); }
        debug(): any { return { ...super.debug(), documentation: debugHelper(this.documentation), expression: debugHelper(this.expression) }; }
    }
    export class ConstructorDefinition extends AbstractBase {
        readonly type: Type.ConstructorDefinition;
        constructor(cursor: Cursor) { super(cursor); this.type = Type.ConstructorDefinition; }
        static read(cursor: Cursor): ConstructorDefinition { return new ConstructorDefinition(cursor); }
        get ident(): Token.Ident { return Token.Ident.read(this.lazyObjectData.seek(40)); }
        get arguments(): Iterable<ArgumentDefinition> { return this.lazyObjectData.seek(91).readSequence((element: Cursor) => ArgumentDefinition.read(element), 39); }
        get block(): Iterable<ArgumentDefinitionLine> { return this.lazyObjectData.seek(95).readSequence((element: Cursor) => ArgumentDefinitionLine.read(element), 45); }
        debug(): any { return { ...super.debug(), ident: debugHelper(this.ident), arguments: debugHelper(this.arguments), block: debugHelper(this.block) }; }
    }
    export type Tree = Invalid | BodyBlock | ArgumentBlockApplication | OperatorBlockApplication | Ident | Number | Wildcard | AutoScope | TextLiteral | App | NamedApp | DefaultApp | OprApp | UnaryOprApp | OprSectionBoundary | TemplateFunction | MultiSegmentApp | TypeDef | Assignment | Function | ForeignFunction | Import | Export | Group | TypeSignature | TypeAnnotated | CaseOf | Lambda | Array | Tuple | Annotated | AnnotatedBuiltin | Documented | ConstructorDefinition;
    export function read(cursor: Cursor): Tree { switch (cursor.readU32()) {
        case 0: return new Invalid(cursor.seek(4));
        case 1: return new BodyBlock(cursor.seek(4));
        case 2: return new ArgumentBlockApplication(cursor.seek(4));
        case 3: return new OperatorBlockApplication(cursor.seek(4));
        case 4: return new Ident(cursor.seek(4));
        case 5: return new Number(cursor.seek(4));
        case 6: return new Wildcard(cursor.seek(4));
        case 7: return new AutoScope(cursor.seek(4));
        case 8: return new TextLiteral(cursor.seek(4));
        case 9: return new App(cursor.seek(4));
        case 10: return new NamedApp(cursor.seek(4));
        case 11: return new DefaultApp(cursor.seek(4));
        case 12: return new OprApp(cursor.seek(4));
        case 13: return new UnaryOprApp(cursor.seek(4));
        case 14: return new OprSectionBoundary(cursor.seek(4));
        case 15: return new TemplateFunction(cursor.seek(4));
        case 16: return new MultiSegmentApp(cursor.seek(4));
        case 17: return new TypeDef(cursor.seek(4));
        case 18: return new Assignment(cursor.seek(4));
        case 19: return new Function(cursor.seek(4));
        case 20: return new ForeignFunction(cursor.seek(4));
        case 21: return new Import(cursor.seek(4));
        case 22: return new Export(cursor.seek(4));
        case 23: return new Group(cursor.seek(4));
        case 24: return new TypeSignature(cursor.seek(4));
        case 25: return new TypeAnnotated(cursor.seek(4));
        case 26: return new CaseOf(cursor.seek(4));
        case 27: return new Lambda(cursor.seek(4));
        case 28: return new Array(cursor.seek(4));
        case 29: return new Tuple(cursor.seek(4));
        case 30: return new Annotated(cursor.seek(4));
        case 31: return new AnnotatedBuiltin(cursor.seek(4));
        case 32: return new Documented(cursor.seek(4));
        case 33: return new ConstructorDefinition(cursor.seek(4));
        default: throw new Error("Unexpected discriminant while deserializing.");
    } }
}
export type Tree = Tree.Tree;
export class FractionalDigits extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): FractionalDigits { return new FractionalDigits(cursor); }
    get dot(): Token.Operator { return Token.Operator.read(this.lazyObjectData); }
    get digits(): Token.Digits { return Token.Digits.read(this.lazyObjectData.seek(40)); }
    debug(): any { return { ...super.debug(), dot: debugHelper(this.dot), digits: debugHelper(this.digits) }; }
}
export class MultiSegmentAppSegment extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): MultiSegmentAppSegment { return new MultiSegmentAppSegment(cursor); }
    get header(): Token { return Token.read(this.lazyObjectData.readPointer()); }
    get body(): Tree | null { return this.lazyObjectData.seek(4).readOption((element: Cursor) => Tree.read(element.readPointer())); }
    debug(): any { return { ...super.debug(), header: debugHelper(this.header), body: debugHelper(this.body) }; }
}
export class Line extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): Line { return new Line(cursor); }
    get newline(): Token.Newline { return Token.Newline.read(this.lazyObjectData); }
    get expression(): Tree | null { return this.lazyObjectData.seek(40).readOption((element: Cursor) => Tree.read(element.readPointer())); }
    debug(): any { return { ...super.debug(), newline: debugHelper(this.newline), expression: debugHelper(this.expression) }; }
}
export class ArgumentType extends LazyObject {
    constructor(cursor: Cursor) { super(cursor); }
    static read(cursor: Cursor): ArgumentType { return new ArgumentType(cursor); }
    get operator(): Token.Operator { return Token.Operator.read(this.lazyObjectData); }
    get typeNode(): Tree { return Tree.read(this.lazyObjectData.seek(40).readPointer()); }
    debug(): any { return { ...super.debug(), operator: debugHelper(this.operator), typeNode: debugHelper(this.typeNode) }; }
}
