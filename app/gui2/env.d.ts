/// <reference types="vite/client" />

declare const PROJECT_MANAGER_URL: string

// This is an augmentation to the built-in `ImportMeta` interface.
// This file MUST NOT contain any top-level imports.
interface ImportMeta {
  vitest: typeof import('vitest') | undefined
}
