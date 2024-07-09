/** @file Type definitions common between all backends. */

import type * as React from 'react'

import type * as backend from 'enso-common/src/services/Backend'

export * from 'enso-common/src/services/Backend'
export { default } from 'enso-common/src/services/Backend'

declare module 'enso-common/src/services/Backend' {
  /** Information required to open a project. */
  export interface ProjectStartupInfo {
    // This MUST BE optional because it is lost when `JSON.stringify`ing to put in `localStorage`.
    readonly setProjectAsset?: React.Dispatch<React.SetStateAction<backend.ProjectAsset>>
  }
}
