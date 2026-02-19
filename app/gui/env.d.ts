/**
 * @file Globals defined outside of TypeScript files.
 * These are from variables defined at build time, environment variables,
 * monkeypatching on `window` and generated code.
 */
/// <reference types="vite/client" />
import type { $Config } from './src/config'
import type { ElectronApi } from './src/electronApi'
import type { FeatureFlags } from './src/providers/featureFlags'

// JSDocs here are intentionally empty as these interfaces originate from elsewhere.
declare global {
  const $config: $Config

  interface Window {
    readonly api?: ElectronApi
    // Keep feature flags globals separate from Electron API bundle.
    readonly featureFlags: FeatureFlags
    readonly setFeatureFlags: (flags: Partial<FeatureFlags>) => void
    /**
     * Feature flags that override the default or stored feature flags.
     * This is used by integration tests to set feature flags.
     */
    readonly overrideFeatureFlags?: Partial<FeatureFlags>
  }

  interface LogEvent {
    (message: string, projectId?: string | null, metadata?: object | null): void
  }
}

// Add additional types for svg imports from `#/assets/*.svg`
declare module 'vite/client' {
  declare module '#/assets/*.svg' {
    /**
     * @deprecated Prefer defined keys over importing from `#/assets/*.svg
     */
    const src: string
    export default src
  }
}

declare global {
  const URL: {
    /**
     *  @deprecated use {@link urlParse} to avoid issues during tests.
     */
    parse(url: string | URL, base?: string | URL): URL | null
  }
}
