class LazyObject {
  protected readonly blob: Uint8Array
  protected readonly address: number
}

export module Tree {
  export type Tree = Ident | NamedApp

  export enum Type {
    Ident = 0,
    NamedApp = 1
  }

  class Base extends LazyObject {
    get span(): Span {
      throw new Error("TODO")
    }

    children(): Iterable<Tree> {
      throw new Error("TODO")
    }
  }

  export class Ident extends Base {
    readonly type: Type.Ident

    get token(): Token.Ident {
      throw new Error("TODO")
    }
  }

  export class NamedApp extends Base {
    readonly type: Type.NamedApp

    get func(): Tree {
      throw new Error("TODO")
    }

    get open(): Token.OpenSymbol | undefined {
      throw new Error("TODO")
    }

    get name(): Token.Ident {
      throw new Error("TODO")
    }

    get equals(): Token.Operator {
      throw new Error("TODO")
    }

    get arg(): Tree {
      throw new Error("TODO")
    }

    get close(): Token.CloseSymbol | undefined {
      throw new Error("TODO")
    }
  }
}
export type Tree = Tree.Tree

function switchTree(tree: Tree) {
  const c = tree.type
  const span = tree.span
  switch (c) {
    case Tree.Type.Ident:
      return "ident"
    case Tree.Type.NamedApp:
      return "named app"
    default:
      const _ = c satisfies never
  }
}

export class Span extends LazyObject {
  get start(): number {
    throw new Error("TODO")
  }

  get len(): number {
    throw new Error("TODO")
  }
}

export type Token = Token.Ident | Token.Operator | Token.OpenSymbol | Token.CloseSymbol

export module Token {
  export enum Type {
    Ident,
    Operator,
    OpenSymbol,
    CloseSymbol
  }

  class Base {
    protected readonly blob: Uint8Array
    protected readonly address: number
    readonly type: Type

    get span(): Span {
      throw new Error("TODO")
    }
  }

  export class Ident extends Base {
    readonly type: Type.Ident

    get isTypeOrConstructor(): boolean {
      throw new Error("TODO")
    }
  }

  export class Operator extends Base {
    readonly type: Type.Operator
  }

  export class OpenSymbol extends Base {
    readonly type: Type.OpenSymbol
  }

  export class CloseSymbol extends Base {
    readonly type: Type.CloseSymbol
  }
}
