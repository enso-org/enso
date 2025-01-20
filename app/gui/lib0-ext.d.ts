/** @file Fixups for too loose typings present in lib0. */

import 'lib0/set'

declare module 'lib0/set' {
  function first<T>(set: Set<T>): T | undefined
}
