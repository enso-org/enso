import type { Path, UUID } from './types.js'

const PROJECT_IDENTITY_SEPARATOR = '\0'
const LOCAL_PROJECT_KEY_PREFIX = 'local-'

function byteToHex(byte: number) {
  return byte.toString(16).padStart(2, '0')
}

/** Create cache key for project-service local identity. */
export function makeProjectCacheKey(params: {
  readonly projectId: UUID | string
  readonly projectsDirectory: Path | string
}) {
  return `${params.projectsDirectory}${PROJECT_IDENTITY_SEPARATOR}${params.projectId}`
}

/** Create opaque cache key used for logs and telemetry. */
export async function makeProjectTelemetryKey(
  projectsDirectory: Path | string,
  projectId: UUID | string,
) {
  const crypto = globalThis.crypto
  if (crypto == null || crypto.subtle == null) {
    throw new Error('Web Crypto API unavailable.')
  }
  const input = new TextEncoder().encode(makeProjectCacheKey({ projectsDirectory, projectId }))
  const digest = await crypto.subtle.digest('SHA-256', input)
  const hex = Array.from(new Uint8Array(digest), byteToHex).join('')
  return `${LOCAL_PROJECT_KEY_PREFIX}${hex}`
}
