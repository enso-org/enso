/**
 * @file Type definitions for modules that currently lack typings on DefinitelyTyped.
 *
 * This file MUST NOT `export {}` so that the modules are visible to other files.
 */

declare module 'create-servers' {
  import type * as http from 'node:http'
  import type * as https from 'node:https'

  /** Configuration options for `create-servers`. */
  interface CreateServersOptions {
    readonly http?: number
    readonly handler: http.RequestListener
    // eslint-disable-next-line no-restricted-syntax
    readonly https?: {
      readonly port: number
      readonly key: string
      readonly cert: string
    }
  }

  /** An error passed to a callback when a HTTP request fails. */
  interface HttpError {
    readonly http: string
  }

  /** Created server instances of various types. */
  interface CreatedServers {
    readonly http?: http.Server
    readonly https?: https.Server
  }

  export default function (
    option: CreateServersOptions,
    // The types come from a third-party API and cannot be changed.
    // eslint-disable-next-line no-restricted-syntax
    handler: (err: HttpError | undefined, servers: CreatedServers) => void,
  ): unknown
}
