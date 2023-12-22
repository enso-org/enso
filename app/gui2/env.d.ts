/// <reference types="vite/client" />

declare const PROJECT_MANAGER_URL: string
declare const RUNNING_VITEST: boolean
declare const IS_CLOUD_BUILD: boolean

interface Document {
  caretPositionFromPoint(x: number, y: number): { offsetNode: Node; offset: number } | null
}
