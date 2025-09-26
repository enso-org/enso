import {
  EmailAddress,
  OrganizationId,
  Path,
  UserId,
  type AnyRealAsset,
  type AssetType,
  type Plan,
  type User,
} from '#/services/Backend'
import { newDirectoryId } from '#/services/LocalBackend'
import * as container from '$/providers/container'
import { expect, test } from 'vitest'
import {
  CLOUD_INITIAL_PROJECT_RELATIVE_PATH,
  initialProjectPath,
  LOCAL_INITIAL_PROJECT_RELATIVE_PATH,
} from '../initialProject'

// keep import registerting local storage key
const _ = container
const CLOUD_ROOT_PATH = 'enso://Users/mock'
const LOCAL_ROOT_PATH = '/home/user/Documents/enso-projects'
const LOCAL_WELCOME_PROJECT = `/home/user/Documents/enso-projects/${LOCAL_INITIAL_PROJECT_RELATIVE_PATH}`
const CLOUD_WELCOME_PROJECT = `enso://Users/mock/${CLOUD_INITIAL_PROJECT_RELATIVE_PATH}`
const USER: User = {
  isEnabled: true,
  isOrganizationAdmin: false,
  rootDirectoryId: newDirectoryId(Path(CLOUD_ROOT_PATH)),
  userGroups: null,
  plan: 'solo',
  isEnsoTeamMember: false,
  organizationId: OrganizationId('organization-Mock'),
  userId: UserId('Mock user'),
  name: 'mock',
  email: EmailAddress('mock@mock.com'),
}

interface AssetEntry {
  type: AssetType
  title: string
}

function mockBackends(plan: Plan, localHome: AssetEntry[] = [], cloudHome: AssetEntry[] = []) {
  return {
    localBackend: {
      rootPath: () => Path(LOCAL_ROOT_PATH),
      listDirectory: () =>
        Promise.resolve({ assets: localHome as AnyRealAsset[], paginationToken: null }),
    },
    remoteBackend: {
      rootPath: () =>
        plan === 'free' || plan === 'solo' ? Path(CLOUD_ROOT_PATH) : Path('enso://'),
      listDirectory: () =>
        Promise.resolve({ assets: cloudHome as AnyRealAsset[], paginationToken: null }),
    },
  }
}

test.each(['free', 'solo', 'team', 'enterprise'] as const)(
  'Initial project from configuration with %s plan',
  async (plan) => {
    const resultFromName = await initialProjectPath('Name', { ...USER, plan }, mockBackends(plan))
    expect(resultFromName).toBe(`${LOCAL_ROOT_PATH}/Name`)
    const resultFromURL = await initialProjectPath(
      'file:///home/user/Name.enso-project',
      { ...USER, plan },
      mockBackends(plan),
    )
    expect(resultFromURL).toBeUndefined()
  },
)

test.each`
  plan            | expected
  ${'free'}       | ${LOCAL_WELCOME_PROJECT}
  ${'solo'}       | ${CLOUD_WELCOME_PROJECT}
  ${'team'}       | ${CLOUD_WELCOME_PROJECT}
  ${'enterprise'} | ${CLOUD_WELCOME_PROJECT}
`('Initial project on fresh install with $plan plan', async ({ plan, expected }) => {
  const result = await initialProjectPath(undefined, { ...USER, plan }, mockBackends(plan))
  expect(result).toBe(expected)
})

test.each([
  {
    localHome: [{ title: 'Samples', type: 'directory' as const }],
    cloudHome: [{ title: 'Samples', type: 'directory' as const }],
    shouldOpen: true,
  },
  {
    localHome: [],
    cloudHome: [{ title: 'Samples', type: 'directory' as const }],
    shouldOpen: true,
  },
  {
    localHome: [{ title: 'Samples', type: 'directory' as const }],
    cloudHome: [],
    shouldOpen: true,
  },
  {
    localHome: [],
    cloudHome: [],
    shouldOpen: true,
  },
  {
    localHome: [
      { title: 'Samples', type: 'directory' as const },
      { title: 'New Project 1', type: 'project' as const },
    ],
    cloudHome: [{ type: 'directory' as const, title: 'Samples' }],
    shouldOpen: false,
  },
  {
    localHome: [{ title: 'Samples', type: 'directory' as const }],
    cloudHome: [
      { type: 'directory' as const, title: 'Samples' },
      { title: 'New Project 1', type: 'project' as const },
    ],
    shouldOpen: false,
  },
  {
    localHome: [{ title: 'Samples', type: 'project' as const }],
    cloudHome: [{ title: 'Samples', type: 'directory' as const }],
    shouldOpen: false,
  },
  {
    localHome: [{ title: 'Samples', type: 'directory' as const }],
    cloudHome: [{ title: 'Samples', type: 'project' as const }],
    shouldOpen: false,
  },
])(
  'Initial project with homes $localHome and $cloudHome',
  async ({ localHome, cloudHome, shouldOpen }) => {
    const result = await initialProjectPath(
      undefined,
      USER,
      mockBackends(USER.plan, localHome, cloudHome),
    )
    if (shouldOpen) {
      expect(result).toBe(CLOUD_WELCOME_PROJECT)
    } else {
      expect(result).toBeFalsy()
    }
  },
)
