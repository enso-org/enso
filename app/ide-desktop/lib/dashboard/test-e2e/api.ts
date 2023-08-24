/** @file The mock API. */
import type * as test from '@playwright/test'

import type * as backend from '../src/authentication/src/dashboard/backend'
import * as config from '../src/authentication/src/config'
import * as dateTime from '../src/authentication/src/dashboard/dateTime'
import type * as remoteBackend from '../src/authentication/src/dashboard/remoteBackend'
import * as remoteBackendPaths from '../src/authentication/src/dashboard/remoteBackendPaths'

// =================
// === Constants ===
// =================

/** The HTTP status code representing a bad request. */
const HTTP_STATUS_BAD_REQUEST = 400
/* eslint-disable no-restricted-syntax */
/** A directory ID that is a path glob. */
const GLOB_DIRECTORY_ID = '*' as backend.DirectoryId
/** A projet ID that is a path glob. */
const GLOB_PROJECT_ID = '*' as backend.ProjectId
/** A tag ID that is a path glob. */
const GLOB_FILE_ID = '*' as backend.FileId
/** A tag ID that is a path glob. */
const GLOB_SECRET_ID = '*' as backend.SecretId
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
    await page.route(BASE_URL + '**', (_route, request) => {
        throw new Error(`Missing route handler for '${request.url().replace(BASE_URL, '')}'.`)
    })

    // === Endpoints returning arrays ===

    await page.route(BASE_URL + remoteBackendPaths.LIST_DIRECTORY_PATH + '*', async route => {
        await route.fulfill({
            json: { assets: [] } satisfies remoteBackend.ListDirectoryResponseBody,
        })
    })
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

    // === Endpoints returning `void` ===

    await page.route(BASE_URL + remoteBackendPaths.INVITE_USER_PATH + '*', async route => {
        await route.fulfill()
    })
    await page.route(BASE_URL + remoteBackendPaths.CREATE_PERMISSION_PATH + '*', async route => {
        await route.fulfill()
    })
    await page.route(
        BASE_URL + remoteBackendPaths.deleteDirectoryPath(GLOB_DIRECTORY_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackendPaths.closeProjectPath(GLOB_PROJECT_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackendPaths.openProjectPath(GLOB_PROJECT_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackendPaths.deleteProjectPath(GLOB_PROJECT_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackendPaths.deleteFilePath(GLOB_FILE_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackendPaths.deleteSecretPath(GLOB_SECRET_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackendPaths.deleteTagPath(GLOB_TAG_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )

    // === Other endpoints ===
    const defaultUser: backend.UserOrOrganization = {
        // eslint-disable-next-line no-restricted-syntax
        email: 'email@example.com' as backend.EmailAddress,
        name: 'user name',
        // eslint-disable-next-line no-restricted-syntax
        id: 'placeholder organization id' as backend.UserOrOrganizationId,
        isEnabled: true,
    }
    let currentUser: backend.UserOrOrganization | null = defaultUser
    await page.route(
        BASE_URL + remoteBackendPaths.CREATE_USER_PATH + '*',
        async (route, request) => {
            if (request.method() === 'POST') {
                // The type of the body sent by this app is statically known.
                // eslint-disable-next-line @typescript-eslint/no-unsafe-assignment
                const body: backend.CreateUserRequestBody = await request.postDataJSON()
                currentUser = {
                    email: body.userEmail,
                    name: body.userName,
                    id: body.organizationId ?? defaultUser.id,
                    isEnabled: false,
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

    return {
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
