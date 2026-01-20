/**
 * @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 */

import type { ElectronApi } from 'enso-gui/src/electronApi'

// JSDocs here are intentionally empty as these interfaces originate from elsewhere.
declare global {
  // Documentation is already inherited.
  /** */
  interface Window {
    readonly api: ElectronApi
  }

  interface ImportMetaEnv {
    readonly [key: string]: string | undefined
  }

  interface ImportMeta {
    readonly env: ImportMetaEnv
  }

  namespace NodeJS {
    /** Environment variables. */
    interface ProcessEnv {
      readonly [key: string]: never

      // This is declared in `@types/node`. It MUST be re-declared here to suppress the error
      // about this property conflicting with the index signature above.
      // MUST NOT be `readonly`, or else `@types/node` will error.
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      TZ?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly CI?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly PROD?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly CSC_LINK?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly APPLEID?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly APPLEIDPASS?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly APPLETEAMID?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ENSO_BUILD_ELECTRON_BUILDER_CONFIG?: string
      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly npm_package_name?: string

      // === Integration test variables ===

      readonly ENSO_TEST?: string
      readonly ENSO_TEST_PROJECTS_DIR?: string
      readonly ENSO_TEST_APP_ARGS?: string
      readonly ENSO_TEST_USER?: string
      ENSO_TEST_EXEC_PATH?: string

      // === Electron watch script variables ===

      // @ts-expect-error The index signature is intentional to disallow unknown env vars.
      readonly ELECTRON_DEV_MODE?: string
    }
  }
}

export {}
