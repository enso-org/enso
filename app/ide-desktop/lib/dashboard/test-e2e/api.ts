/** @file The mock API. */
import * as test from '@playwright/test'

import * as backend from '../src/authentication/src/dashboard/backend'
import * as config from '../src/authentication/src/config'
import * as dateTime from '../src/authentication/src/dashboard/dateTime'
import * as remoteBackend from '../src/authentication/src/dashboard/remoteBackend'

// =================
// === Constants ===
// =================

/** A directory ID that is a path glob. */
const GLOB_DIRECTORY_ID = backend.DirectoryId('*')
/** A projet ID that is a path glob. */
const GLOB_PROJECT_ID = backend.ProjectId('*')
/** A tag ID that is a path glob. */
const GLOB_FILE_ID = backend.FileId('*')
/** A tag ID that is a path glob. */
const GLOB_SECRET_ID = backend.SecretId('*')
/** A tag ID that is a path glob. */
const GLOB_TAG_ID = backend.TagId('*')
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

    await page.route(BASE_URL + remoteBackend.LIST_DIRECTORY_PATH + '*', async route => {
        await route.fulfill({
            json: { assets: [] } satisfies remoteBackend.ListDirectoryResponseBody,
        })
    })
    await page.route(BASE_URL + remoteBackend.LIST_FILES_PATH + '*', async route => {
        await route.fulfill({
            json: { files: [] } satisfies remoteBackend.ListFilesResponseBody,
        })
    })
    await page.route(BASE_URL + remoteBackend.LIST_PROJECTS_PATH + '*', async route => {
        await route.fulfill({
            json: { projects: [] } satisfies remoteBackend.ListProjectsResponseBody,
        })
    })
    await page.route(BASE_URL + remoteBackend.LIST_SECRETS_PATH + '*', async route => {
        await route.fulfill({
            json: { secrets: [] } satisfies remoteBackend.ListSecretsResponseBody,
        })
    })
    await page.route(BASE_URL + remoteBackend.LIST_TAGS_PATH + '*', async route => {
        await route.fulfill({
            json: { tags: [] } satisfies remoteBackend.ListTagsResponseBody,
        })
    })
    await page.route(BASE_URL + remoteBackend.LIST_USERS_PATH + '*', async route => {
        await route.fulfill({
            json: { users: [] } satisfies remoteBackend.ListUsersResponseBody,
        })
    })
    await page.route(BASE_URL + remoteBackend.LIST_VERSIONS_PATH + '*', async (route, request) => {
        await route.fulfill({
            json: {
                versions: [
                    {
                        ami: null,
                        created: dateTime.toRfc3339(new Date()),
                        number: {
                            lifecycle: backend.VersionLifecycle.development,
                            value: '2023.2.1-dev',
                        },
                        // eslint-disable-next-line @typescript-eslint/naming-convention, no-restricted-syntax
                        version_type: (new URL(request.url()).searchParams.get('version_type') ??
                            '') as backend.VersionType,
                    } satisfies backend.Version,
                ],
            },
        })
    })

    // === Endpoints returning `void` ===

    await page.route(BASE_URL + remoteBackend.INVITE_USER_PATH + '*', async route => {
        await route.fulfill()
    })
    await page.route(BASE_URL + remoteBackend.CREATE_PERMISSION_PATH + '*', async route => {
        await route.fulfill()
    })
    await page.route(
        BASE_URL + remoteBackend.deleteDirectoryPath(GLOB_DIRECTORY_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackend.closeProjectPath(GLOB_PROJECT_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackend.openProjectPath(GLOB_PROJECT_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(
        BASE_URL + remoteBackend.deleteProjectPath(GLOB_PROJECT_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(BASE_URL + remoteBackend.deleteFilePath(GLOB_FILE_ID) + '*', async route => {
        await route.fulfill()
    })
    await page.route(
        BASE_URL + remoteBackend.deleteSecretPath(GLOB_SECRET_ID) + '*',
        async route => {
            await route.fulfill()
        }
    )
    await page.route(BASE_URL + remoteBackend.deleteTagPath(GLOB_TAG_ID) + '*', async route => {
        await route.fulfill()
    })

    // === Other endpoints ===
    await page.route(BASE_URL + remoteBackend.USERS_ME_PATH + '*', async route => {
        await route.fulfill({
            json: {
                email: backend.EmailAddress('email@example.com'),
                name: 'user name',
                id: backend.UserOrOrganizationId('id'),
                isEnabled: true,
            },
        })
    })
}
