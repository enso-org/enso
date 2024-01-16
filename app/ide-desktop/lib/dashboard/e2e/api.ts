/** @file The mock API. */
import * as test from '@playwright/test'

import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'

import * as backend from '../src/services/backend'
import type * as remoteBackend from '../src/services/remoteBackend'
import * as remoteBackendPaths from '../src/services/remoteBackendPaths'
import * as config from '../src/utilities/config'
import * as dateTime from '../src/utilities/dateTime'
import * as uniqueString from '../src/utilities/uniqueString'

// =================
// === Constants ===
// =================

/** The HTTP status code representing a response with an empty body. */
const HTTP_STATUS_NO_CONTENT = 204
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

/** Parameters for {@link mockApi}. */
interface MockParams {
    page: test.Page
}

/** Add route handlers for the mock API to a page. */
// This syntax is required for Playwright to work properly.
// eslint-disable-next-line no-restricted-syntax
export async function mockApi({ page }: MockParams) {
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

    const deleteAsset = (assetId: backend.AssetId) => {
        deletedAssets.add(assetId)
    }

    const undeleteAsset = (assetId: backend.AssetId) => {
        deletedAssets.delete(assetId)
    }

    const createDirectory = (
        title: string,
        rest: Partial<backend.DirectoryAsset> = {}
    ): backend.DirectoryAsset =>
        object.merge(
            {
                type: backend.AssetType.directory,
                id: backend.DirectoryId('directory-' + uniqueString.uniqueString()),
                projectState: null,
                title,
                modifiedAt: dateTime.toRfc3339(new Date()),
                description: null,
                labels: [],
                parentId: defaultDirectoryId,
                permissions: [],
            },
            rest
        )

    const createProject = (
        title: string,
        rest: Partial<backend.ProjectAsset> = {}
    ): backend.ProjectAsset =>
        object.merge(
            {
                type: backend.AssetType.project,
                id: backend.ProjectId('project-' + uniqueString.uniqueString()),
                projectState: {
                    type: backend.ProjectState.opened,
                    // eslint-disable-next-line @typescript-eslint/naming-convention
                    volume_id: '',
                },
                title,
                modifiedAt: dateTime.toRfc3339(new Date()),
                description: null,
                labels: [],
                parentId: defaultDirectoryId,
                permissions: [],
            },
            rest
        )

    const createFile = (title: string, rest: Partial<backend.FileAsset> = {}): backend.FileAsset =>
        object.merge(
            {
                type: backend.AssetType.file,
                id: backend.FileId('file-' + uniqueString.uniqueString()),
                projectState: null,
                title,
                modifiedAt: dateTime.toRfc3339(new Date()),
                description: null,
                labels: [],
                parentId: defaultDirectoryId,
                permissions: [],
            },
            rest
        )

    const createSecret = (
        title: string,
        rest: Partial<backend.SecretAsset> = {}
    ): backend.SecretAsset =>
        object.merge(
            {
                type: backend.AssetType.secret,
                id: backend.SecretId('secret-' + uniqueString.uniqueString()),
                projectState: null,
                title,
                modifiedAt: dateTime.toRfc3339(new Date()),
                description: null,
                labels: [],
                parentId: defaultDirectoryId,
                permissions: [],
            },
            rest
        )

    const addDirectory = (title: string, rest?: Partial<backend.DirectoryAsset>) => {
        addAsset(createDirectory(title, rest))
    }

    const addProject = (title: string, rest?: Partial<backend.ProjectAsset>) => {
        addAsset(createProject(title, rest))
    }

    const addFile = (title: string, rest?: Partial<backend.FileAsset>) => {
        addAsset(createFile(title, rest))
    }

    const addSecret = (title: string, rest?: Partial<backend.SecretAsset>) => {
        addAsset(createSecret(title, rest))
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

        const isOnline = await page.evaluate(() => navigator.onLine)

        if (!isOnline) {
            await page.route('https://fonts.googleapis.com/*', async route => {
                await route.abort()
            })
        }

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
                filteredAssets.sort(
                    (a, b) => backend.ASSET_TYPE_ORDER[a.type] - backend.ASSET_TYPE_ORDER[b.type]
                )
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
                        /* eslint-disable @typescript-eslint/naming-convention */
                        organizationId: defaultOrganizationId,
                        projectId: backend.ProjectId('project-example id'),
                        name: 'example project name',
                        state: {
                            type: backend.ProjectState.opened,
                            volume_id: '',
                            opened_by: defaultEmail,
                        },
                        packageName: 'Project_root',
                        ide_version: null,
                        engine_version: {
                            value: '2023.2.1-nightly.2023.9.29',
                            lifecycle: backend.VersionLifecycle.development,
                        },
                        /* eslint-enable @typescript-eslint/naming-convention */
                    } satisfies backend.ProjectRaw,
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
            BASE_URL + remoteBackendPaths.deleteAssetPath(GLOB_ASSET_ID),
            async (route, request) => {
                if (request.method() === 'DELETE') {
                    const [, id] = request.url().match(/[/]assets[/]([^?]+)/) ?? []
                    if (id != null) {
                        // eslint-disable-next-line no-restricted-syntax
                        deleteAsset(id as backend.AssetId)
                    }
                    await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
                } else {
                    await route.fallback()
                }
            }
        )
        await page.route(
            BASE_URL + remoteBackendPaths.UNDO_DELETE_ASSET_PATH,
            async (route, request) => {
                if (request.method() === 'PATCH') {
                    /** The type for the JSON request payload for this endpoint. */
                    interface Body {
                        assetId: backend.AssetId
                    }
                    // The type of the body sent by this app is statically known.
                    // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                    const body: Body = await request.postDataJSON()
                    undeleteAsset(body.assetId)
                    await route.fulfill({ status: HTTP_STATUS_NO_CONTENT })
                } else {
                    await route.fallback()
                }
            }
        )
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
            } else {
                await route.fallback()
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
                    permissions: [
                        {
                            user: {
                                pk: backend.Subject(''),
                                /* eslint-disable @typescript-eslint/naming-convention */
                                user_name: defaultUsername,
                                user_email: defaultEmail,
                                organization_id: defaultOrganizationId,
                                /* eslint-enable @typescript-eslint/naming-convention */
                            },
                            permission: permissions.PermissionAction.own,
                        },
                    ],
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
        addAsset,
        deleteAsset,
        undeleteAsset,
        createDirectory,
        createProject,
        createFile,
        createSecret,
        addDirectory,
        addProject,
        addFile,
        addSecret,
    }
}