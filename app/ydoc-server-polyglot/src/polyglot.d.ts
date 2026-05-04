/** @file Type declarations for environment provided in polyglot JVM runtime. */

import type { YjsChannelServer } from '../../ydoc-channel/dist/YjsChannel'

declare class WebSocketServer {
  constructor(config: any)
  onconnect: ((socket: any, url: any) => any) | null
  start(): void
}

declare class Java {
  static type(name: string): any
}

declare const YDOC_HOST: string | undefined
declare const YDOC_PORT: number | undefined
declare const YDOC_LS_DEBUG: boolean | undefined
declare const YDOC_JSON_CHANNEL_CALLBACKS: YjsChannelServer | undefined
declare const YDOC_BINARY_CHANNEL_CALLBACKS: YjsChannelServer | undefined
declare const YDOC_VIS_CONTROL_CHANNEL_CALLBACKS: YjsChannelServer<string> | undefined
declare const YDOC_VIS_DATA_CHANNEL_CALLBACKS: YjsChannelServer<any> | undefined

// rust ffi shims
declare function parse_block(code: string): Uint8Array
declare function parse_module(code: string): Uint8Array
declare function is_ident_or_operator(code: string): number
declare function self_arg_separator(code: string): number
declare function is_numeric_literal(code: string): boolean
declare function xxHash128(input: IDataType): string
