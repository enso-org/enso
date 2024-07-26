/** @file Utilities getting various metadata about the app. */
import * as common from 'enso-common'
import * as detect from 'enso-common/src/detect'

/* eslint-disable @typescript-eslint/naming-convention */

// =================
// === Constants ===
// =================

const ONE_HOUR_MS = 3_600_000
export const LATEST_RELEASE_PAGE_URL = 'https://github.com/enso-org/enso/releases/latest'

// ==================
// === GitHub API ===
// ==================

/** Metadata for a GitHub user. */
interface GitHubSimpleUser {
  readonly name?: string
  readonly email?: string
  readonly login: string
  readonly id: number
  readonly node_id: string
  readonly avatar_url: string
  readonly gravatar_id?: string
  readonly url: string
  readonly html_url: string
  readonly followers_url: string
  readonly following_url: string
  readonly gists_url: string
  readonly starred_url: string
  readonly subscriptions_url: string
  readonly organizations_url: string
  readonly repos_url: string
  readonly events_url: string
  readonly received_events_url: string
  readonly type: string
  readonly site_admin: boolean
  readonly starred_at: string
}

/** State of an asset attached to a GitHub release. */
export enum GitHubReleaseAssetState {
  uploaded = 'uploaded',
  open = 'open',
}

/** Metadata for an asset attached to a GitHub release. */
interface GitHubReleaseAsset {
  readonly url: string
  readonly browser_download_url: string
  readonly id: number
  readonly node_id: string
  readonly name: string
  readonly label?: string
  readonly state: GitHubReleaseAssetState
  readonly content_type: string
  readonly size: number
  readonly download_count: number
  readonly created_at: string
  readonly updated_at: string
  readonly uploader?: GitHubSimpleUser
}

/** Metadata for a GitHub release. */
interface GitHubRelease {
  readonly url: string
  readonly assets_url: string
  readonly upload_url: string
  readonly html_url: string
  readonly tarball_url?: string
  readonly zipball_url?: string
  readonly id: number
  readonly author: GitHubSimpleUser
  readonly node_id: string
  /** The name of the tag. */
  readonly tag_name: string
  /** Specifies the commitish value that determines where the Git tag is created from. */
  readonly target_commitish: string
  readonly name?: string
  readonly body?: string
  /** `true` to create a draft (unpublished) release, `false` to create a published one. */
  readonly draft: boolean
  /** Whether to identify the release as a prerelease or a full release. */
  readonly prerelease: boolean
  readonly created_at: string
  readonly published_at: string
  readonly assets: GitHubReleaseAsset[]
}

// =====================
// === CachedRelease ===
// =====================

/** Metadata for a GitHub release, plus metadata for caching purposes. */
interface CachedRelease {
  readonly lastFetchEpochMs: number
  readonly gitHubRelease: GitHubRelease
}

const LOCAL_STORAGE_KEY = `${common.PRODUCT_NAME.toLowerCase()}-cached-release`

/** Gets the metadata for the latest release of the app. */
export async function getLatestRelease() {
  const savedCachedRelease = localStorage.getItem(LOCAL_STORAGE_KEY)
  // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
  const cachedRelease: CachedRelease | null =
    savedCachedRelease != null ? JSON.parse(savedCachedRelease) : null
  if (cachedRelease != null && Number(new Date()) - cachedRelease.lastFetchEpochMs < ONE_HOUR_MS) {
    return cachedRelease.gitHubRelease
  } else {
    const response = await fetch('https://api.github.com/repos/enso-org/enso/releases/latest', {
      headers: [
        ['Accept', 'application/vnd.github+json'],
        ['X-GitHub-Api-Version', '2022-11-28'],
      ],
    })
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const data: GitHubRelease = await response.json()
    localStorage.setItem(
      LOCAL_STORAGE_KEY,
      JSON.stringify({
        lastFetchEpochMs: Number(new Date()),
        gitHubRelease: data,
      } satisfies CachedRelease),
    )
    return data
  }
}

const appExtension = (() => {
  switch (detect.platform()) {
    case detect.Platform.macOS:
      return '.dmg'
    case detect.Platform.windows:
      return '.exe'
    default:
      // assume Unix-like.
      return '.AppImage'
  }
})()

/** Gets the download URL of the latest release of the app. */
export async function getDownloadUrl() {
  const assets = (await getLatestRelease()).assets
  return (
    assets.find((item) => item.browser_download_url.endsWith(appExtension))?.browser_download_url ??
    null
  )
}
