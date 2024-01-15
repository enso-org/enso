/** @file The mock API. */
import * as test from '@playwright/test'

import * as backend from '../src/services/backend'
import type * as remoteBackend from '../src/services/remoteBackend'
import * as remoteBackendPaths from '../src/services/remoteBackendPaths'
import * as config from '../src/utilities/config'
import * as dateTime from '../src/utilities/dateTime'
import * as uniqueString from '../src/utilities/uniqueString'

// =================
// === Constants ===
// =================

/** The HTTP status code representing a bad request. */
const HTTP_STATUS_BAD_REQUEST = 400
/** The HTTP status code representing a URL that does not exist. */
const HTTP_STATUS_NOT_FOUND = 404
/* eslint-disable no-restricted-syntax */
/** An asset ID that is a path glob. */
const GLOB_ASSET_ID = '*' as backend.AssetId
/** A projet ID that is a path glob. */
const GLOB_PROJECT_ID = '*' as backend.ProjectId
/** A tag ID that is a path glob. */
const GLOB_TAG_ID = '*' as backend.TagId
/* eslint-enable no-restricted-syntax */
/** The base URL for all backend endpoints. */
const BASE_URL = config.ACTIVE_CONFIG.apiUrl + '/'

// ===============
// === mockApi ===
// ===============

/** Add route handlers for the mock API to a page. */
export async function mockApi(page: test.Page) {
    // eslint-disable-next-line no-restricted-syntax
    const defaultEmail = 'email@example.com' as backend.EmailAddress
    const defaultUsername = 'user name'
    const defaultOrganizationId = backend.UserOrOrganizationId('organization-placeholder id')
    const defaultDirectoryId = backend.DirectoryId('directory-placeholder id')
    const defaultUser: backend.UserOrOrganization = {
        email: defaultEmail,
        name: defaultUsername,
        id: defaultOrganizationId,
        isEnabled: true,
        rootDirectoryId: defaultDirectoryId,
    }
    let currentUser: backend.UserOrOrganization | null = defaultUser
    const assetMap = new Map<backend.AssetId, backend.AnyAsset>()
    const deletedAssets = new Set<backend.AssetId>()
    const assets: backend.AnyAsset[] = []

    const addAsset = (asset: backend.AnyAsset) => {
        assets.push(asset)
        assetMap.set(asset.id, asset)
    }

    // This will be used in the future.
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    const deleteAsset = (assetId: backend.AssetId) => {
        deletedAssets.add(assetId)
    }

    await test.test.step('Mock API', async () => {
        await page.route('https://www.google-analytics.com/**', async route => {
            await route.fulfill()
        })

        await page.route('https://www.googletagmanager.com/gtag/js*', async route => {
            await route.fulfill({
                contentType: 'text/javascript',
                body: 'export {};',
            })
        })

        await page.route(BASE_URL + '**', (_route, request) => {
            throw new Error(`Missing route handler for '${request.url().replace(BASE_URL, '')}'.`)
        })

        // === Endpoints returning arrays ===

        await page.route(
            BASE_URL + remoteBackendPaths.LIST_DIRECTORY_PATH + '*',
            async (route, request) => {
                /** The type for the search query for this endpoint. */
                interface Query {
                    /* eslint-disable @typescript-eslint/naming-convention */
                    parent_id?: string
                    filter_by?: backend.FilterBy
                    labels?: backend.LabelName[]
                    recent_projects?: boolean
                    /* eslint-enable @typescript-eslint/naming-convention */
                }
                // The type of the body sent by this app is statically known.
                // eslint-disable-next-line no-restricted-syntax
                const body = Object.fromEntries(
                    new URL(request.url()).searchParams.entries()
                ) as unknown as Query
                const parentId = body.parent_id ?? defaultDirectoryId
                let filteredAssets = assets.filter(asset => asset.parentId === parentId)
                // This lint rule is broken; there is clearly a case for `undefined` below.
                // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
                switch (body.filter_by) {
                    case backend.FilterBy.active: {
                        filteredAssets = filteredAssets.filter(
                            asset => !deletedAssets.has(asset.id)
                        )
                        break
                    }
                    case backend.FilterBy.trashed: {
                        filteredAssets = filteredAssets.filter(asset => deletedAssets.has(asset.id))
                        break
                    }
                    case backend.FilterBy.recent: {
                        filteredAssets = assets
                            .filter(asset => !deletedAssets.has(asset.id))
                            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                            .slice(0, 10)
                        break
                    }
                    case backend.FilterBy.all:
                    case null: {
                        // do nothing
                        break
                    }
                    // eslint-disable-next-line no-restricted-syntax
                    case undefined: {
                        // do nothing
                        break
                    }
                }
                await route.fulfill({
                    json: {
                        assets: filteredAssets,
                    } satisfies remoteBackend.ListDirectoryResponseBody,
                })
            }
        )
        await page.route(BASE_URL + remoteBackendPaths.LIST_FILES_PATH + '*', async route => {
            await route.fulfill({
                json: { files: [] } satisfies remoteBackend.ListFilesResponseBody,
            })
        })
        await page.route(BASE_URL + remoteBackendPaths.LIST_PROJECTS_PATH + '*', async route => {
            await route.fulfill({
                json: { projects: [] } satisfies remoteBackend.ListProjectsResponseBody,
            })
        })
        await page.route(BASE_URL + remoteBackendPaths.LIST_SECRETS_PATH + '*', async route => {
            await route.fulfill({
                json: { secrets: [] } satisfies remoteBackend.ListSecretsResponseBody,
            })
        })
        await page.route(BASE_URL + remoteBackendPaths.LIST_TAGS_PATH + '*', async route => {
            await route.fulfill({
                json: { tags: [] } satisfies remoteBackend.ListTagsResponseBody,
            })
        })
        await page.route(BASE_URL + remoteBackendPaths.LIST_USERS_PATH + '*', async route => {
            await route.fulfill({
                json: { users: [] } satisfies remoteBackend.ListUsersResponseBody,
            })
        })
        await page.route(
            BASE_URL + remoteBackendPaths.LIST_VERSIONS_PATH + '*',
            async (route, request) => {
                await route.fulfill({
                    json: {
                        versions: [
                            {
                                ami: null,
                                created: dateTime.toRfc3339(new Date()),
                                number: {
                                    lifecycle:
                                        // eslint-disable-next-line no-restricted-syntax
                                        'Development' satisfies `${backend.VersionLifecycle.development}` as backend.VersionLifecycle.development,
                                    value: '2023.2.1-dev',
                                },
                                // eslint-disable-next-line @typescript-eslint/naming-convention, no-restricted-syntax
                                version_type: (new URL(request.url()).searchParams.get(
                                    'version_type'
                                ) ?? '') as backend.VersionType,
                            } satisfies backend.Version,
                        ],
                    },
                })
            }
        )

        // === Unimplemented endpoints ===

        await page.route(
            BASE_URL + remoteBackendPaths.getProjectDetailsPath(GLOB_PROJECT_ID),
            async route => {
                await route.fulfill({
                    json: {
                        organizationId: 'example organization id',
                        projectId: 'example project id',
                        name: 'example project name',
                        state: {
                            type: 'OpenInProgress',
                        },
                        packageName: 'Project_root',
                        ideVersion: null,
                        engineVersion: {
                            value: '2023.2.1-nightly.2023.9.29',
                            lifecycle: 'Development',
                        },
                        openedBy: 'email@email.email',
                    },
                })
            }
        )

        // === Endpoints returning `void` ===

        await page.route(
            BASE_URL + remoteBackendPaths.copyAssetPath(GLOB_ASSET_ID),
            async (route, request) => {
                /** The type for the JSON request payload for this endpoint. */
                interface Body {
                    parentDirectoryId: backend.DirectoryId
                }
                const assetId = request.url().match(/assets[/](.+?)[/]copy/)?.[1]
                // eslint-disable-next-line no-restricted-syntax
                const asset = assetId != null ? assetMap.get(assetId as backend.AssetId) : null
                if (asset == null) {
                    if (assetId == null) {
                        await route.fulfill({
                            status: HTTP_STATUS_BAD_REQUEST,
                            json: { error: 'Invalid Asset ID' },
                        })
                    } else {
                        await route.fulfill({
                            status: HTTP_STATUS_NOT_FOUND,
                            json: { error: 'Asset does not exist' },
                        })
                    }
                } else {
                    // The type of the body sent by this app is statically known.
                    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                    const body: Body = await request.postDataJSON()
                    const parentId = body.parentDirectoryId
                    // Can be any asset ID.
                    const id = backend.DirectoryId(uniqueString.uniqueString())
                    const json: backend.CopyAssetResponse = {
                        asset: {
                            id,
                            parentId,
                            title: asset.title + ' (copy)',
                        },
                    }
                    const newAsset = { ...asset }
                    newAsset.id = id
                    newAsset.parentId = parentId
                    newAsset.title += ' (copy)'
                    addAsset(newAsset)
                    await route.fulfill({ json })
                }
            }
        )
        await page.route(BASE_URL + remoteBackendPaths.INVITE_USER_PATH + '*', async route => {
            await route.fulfill()
        })
        await page.route(
            BASE_URL + remoteBackendPaths.CREATE_PERMISSION_PATH + '*',
            async route => {
                await route.fulfill()
            }
        )
        await page.route(
            BASE_URL + remoteBackendPaths.deleteAssetPath(GLOB_ASSET_ID),
            async route => {
                await route.fulfill()
            }
        )
        await page.route(
            BASE_URL + remoteBackendPaths.closeProjectPath(GLOB_PROJECT_ID),
            async route => {
                await route.fulfill()
            }
        )
        await page.route(
            BASE_URL + remoteBackendPaths.openProjectPath(GLOB_PROJECT_ID),
            async route => {
                await route.fulfill()
            }
        )
        await page.route(BASE_URL + remoteBackendPaths.deleteTagPath(GLOB_TAG_ID), async route => {
            await route.fulfill()
        })

        // === Other endpoints ===
        await page.route(
            BASE_URL + remoteBackendPaths.CREATE_USER_PATH + '*',
            async (route, request) => {
                if (request.method() === 'POST') {
                    // The type of the body sent by this app is statically known.
                    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                    const body: backend.CreateUserRequestBody = await request.postDataJSON()
                    const id = body.organizationId ?? defaultUser.id
                    const rootDirectoryId = backend.DirectoryId(
                        id.replace(/^organization-/, 'directory-')
                    )
                    currentUser = {
                        email: body.userEmail,
                        name: body.userName,
                        id,
                        isEnabled: false,
                        rootDirectoryId,
                    }
                    await route.fulfill({ json: currentUser })
                } else if (request.method() === 'GET') {
                    if (currentUser != null) {
                        await route.fulfill({ json: [] })
                    } else {
                        await route.fulfill({ status: HTTP_STATUS_BAD_REQUEST })
                    }
                }
            }
        )
        await page.route(BASE_URL + remoteBackendPaths.USERS_ME_PATH + '*', async route => {
            await route.fulfill({
                json: currentUser,
            })
        })
        await page.route(BASE_URL + remoteBackendPaths.CREATE_TAG_PATH + '*', async route => {
            if (route.request().method() === 'POST') {
                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                const body: backend.CreateTagRequestBody = route.request().postDataJSON()
                const json: backend.Label = {
                    id: backend.TagId(`tag-${uniqueString.uniqueString()}`),
                    value: backend.LabelName(body.value),
                    color: body.color,
                }
                await route.fulfill({ json })
            }
        })
        await page.route(BASE_URL + remoteBackendPaths.CREATE_DIRECTORY_PATH + '*', async route => {
            if (route.request().method() === 'POST') {
                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                const body: backend.CreateDirectoryRequestBody = route.request().postDataJSON()
                const title = body.title
                const id = backend.DirectoryId(`directory-${uniqueString.uniqueString()}`)
                const parentId =
                    body.parentId ?? backend.DirectoryId(`directory-${uniqueString.uniqueString()}`)
                const json: backend.CreatedDirectory = { title, id, parentId }
                addAsset({
                    type: backend.AssetType.directory,
                    description: null,
                    id,
                    labels: [],
                    modifiedAt: dateTime.toRfc3339(new Date()),
                    parentId,
                    permissions: [],
                    projectState: null,
                    title,
                })
                await route.fulfill({ json })
            } else {
                await route.fallback()
            }
        })
    })

    return {
        defaultEmail,
        defaultName: defaultUsername,
        defaultOrganizationId,
        defaultUser,
        /** Returns the current value of `currentUser`. This is a getter, so its return value
         * SHOULD NOT be cached. */
        get currentUser() {
            return currentUser
        },
        setCurrentUser: (user: backend.UserOrOrganization | null) => {
            currentUser = user
        },
    }
}
