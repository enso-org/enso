/** @file Type declarations for environment provided in polyglot JVM runtime. */

declare class WebSocketServer {
  constructor(config: any)
  onconnect: ((socket: any, url: any) => any) | null
  start(): void
}

declare const YDOC_HOST: string | undefined
declare const YDOC_PORT: number | undefined
declare const YDOC_LS_DEBUG: boolean | undefined

// rust ffi shims
declare function parse_block(code: string): Uint8Array
declare function parse_module(code: string): Uint8Array
declare function parse_doc_to_json(docs: string): string
declare function is_ident_or_operator(code: string): number
declare function is_numeric_literal(code: string): boolean
declare function xxHash128(input: IDataType): string
