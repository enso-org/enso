/** @file Utilities getting various metadata about the app. */
import * as common from 'enso-common'

import * as detect from 'enso-common/src/detect'

/* eslint-disable @typescript-eslint/naming-convention */

// =================
// === Constants ===
// =================

const ONE_HOUR_MS = 3_600_000

// ==================
// === GitHub API ===
// ==================

/** Metadata for a GitHub user. */
interface GithubSimpleUser {
    name?: string
    email?: string
    login: string
    id: number
    node_id: string
    avatar_url: string
    gravatar_id?: string
    url: string
    html_url: string
    followers_url: string
    following_url: string
    gists_url: string
    starred_url: string
    subscriptions_url: string
    organizations_url: string
    repos_url: string
    events_url: string
    received_events_url: string
    type: string
    site_admin: boolean
    starred_at: string
}

/** State of an asset attached to a GitHub release. */
export enum GitHubReleaseAssetState {
    uploaded = 'uploaded',
    open = 'open',
}

/** Metadata for an asset attached to a GitHub release. */
interface GitHubReleaseAsset {
    url: string
    browser_download_url: string
    id: number
    node_id: string
    name: string
    label?: string
    state: GitHubReleaseAssetState
    content_type: string
    size: number
    download_count: number
    created_at: string
    updated_at: string
    uploader?: GithubSimpleUser
}

/** Metadata for a GitHub release. */
interface GitHubRelease {
    url: string
    assets_url: string
    upload_url: string
    html_url: string
    tarball_url?: string
    zipball_url?: string
    id: number
    author: GithubSimpleUser
    node_id: string
    /** The name of the tag. */
    tag_name: string
    /** Specifies the commitish value that determines where the Git tag is created from. */
    target_commitish: string
    name?: string
    body?: string
    /** `true` to create a draft (unpublished) release, `false` to create a published one. */
    draft: boolean
    /** Whether to identify the release as a prerelease or a full release. */
    prerelease: boolean
    created_at: string
    published_at: string
    assets: GitHubReleaseAsset[]
}

// =====================
// === CachedRelease ===
// =====================

/** Metadata for a GitHub release, plus metadata for caching purposes. */
interface CachedRelease {
    lastFetchEpochMs: number
    gitHubRelease: GitHubRelease
}

const LOCAL_STORAGE_KEY = `${common.PRODUCT_NAME.toLowerCase()}-cached-release`

/** Gets the metadata for the latest release of the app. */
async function getLatestRelease() {
    const savedCachedRelease = localStorage.getItem(LOCAL_STORAGE_KEY)
    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
    const cachedRelease: CachedRelease | null =
        savedCachedRelease != null ? JSON.parse(savedCachedRelease) : null
    if (
        cachedRelease != null &&
        Number(new Date()) - cachedRelease.lastFetchEpochMs < ONE_HOUR_MS
    ) {
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
            } satisfies CachedRelease)
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
        assets.find(item => item.browser_download_url.endsWith(appExtension))
            ?.browser_download_url ?? null
    )
}
