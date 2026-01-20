import * as container from '$/providers/container'
import {
  type AnyRealAsset,
  AssetType,
  EmailAddress,
  OrganizationId,
  Path,
  Plan,
  type User,
  UserId,
} from 'enso-common/src/services/Backend'
import { newDirectoryId } from 'enso-common/src/services/LocalBackend'
import { expect, test } from 'vitest'
import {
  CLOUD_WELCOME_PROJECT_RELATIVE_PATH,
  LOCAL_WELCOME_PROJECT_RELATIVE_PATH,
  welcomeProjectPath,
} from '../initialProject'

// keep import registerting local storage key
const _ = container
const CLOUD_ROOT_PATH = 'enso://Users/mock'
const LOCAL_ROOT_PATH = '/home/user/Documents/enso-projects'
const LOCAL_WELCOME_PROJECT = `/home/user/Documents/enso-projects/${LOCAL_WELCOME_PROJECT_RELATIVE_PATH}`
const CLOUD_WELCOME_PROJECT = `enso://Users/mock/${CLOUD_WELCOME_PROJECT_RELATIVE_PATH}`
const USER: User = {
  isEnabled: true,
  isOrganizationAdmin: false,
  rootDirectoryId: newDirectoryId(Path(CLOUD_ROOT_PATH)),
  userGroups: null,
  plan: Plan.solo,
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
        plan === Plan.free || plan === Plan.solo ? Path(CLOUD_ROOT_PATH) : Path('enso://'),
      listDirectory: () =>
        Promise.resolve({ assets: cloudHome as AnyRealAsset[], paginationToken: null }),
    },
  }
}

test.each([Plan.free, Plan.solo, Plan.team, Plan.enterprise])(
  'Initial project from configuration with %s plan',
  async (plan) => {
    const resultFromName = await welcomeProjectPath('Name', { ...USER, plan }, mockBackends(plan))
    expect(resultFromName).toBe(`${LOCAL_ROOT_PATH}/Name`)
    const resultFromURL = await welcomeProjectPath(
      'file:///home/user/Name.enso-project',
      { ...USER, plan },
      mockBackends(plan),
    )
    expect(resultFromURL).toBeUndefined()
  },
)

test.each`
  plan               | expected
  ${Plan.free}       | ${LOCAL_WELCOME_PROJECT}
  ${Plan.solo}       | ${CLOUD_WELCOME_PROJECT}
  ${Plan.team}       | ${CLOUD_WELCOME_PROJECT}
  ${Plan.enterprise} | ${CLOUD_WELCOME_PROJECT}
`('Initial project on fresh install with $plan plan', async ({ plan, expected }) => {
  const result = await welcomeProjectPath(undefined, { ...USER, plan }, mockBackends(plan))
  expect(result).toBe(expected)
})

test.each([
  {
    localHome: [{ title: 'Samples', type: AssetType.directory }],
    cloudHome: [{ title: 'Samples', type: AssetType.directory }],
    shouldOpen: true,
  },
  {
    localHome: [],
    cloudHome: [{ title: 'Samples', type: AssetType.directory }],
    shouldOpen: true,
  },
  {
    localHome: [{ title: 'Samples', type: AssetType.directory }],
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
      { title: 'Samples', type: AssetType.directory },
      { title: 'New Project 1', type: AssetType.project },
    ],
    cloudHome: [{ type: AssetType.directory, title: 'Samples' }],
    shouldOpen: false,
  },
  {
    localHome: [{ title: 'Samples', type: AssetType.directory }],
    cloudHome: [
      { type: AssetType.directory, title: 'Samples' },
      { title: 'New Project 1', type: AssetType.project },
    ],
    shouldOpen: false,
  },
  {
    localHome: [{ title: 'Samples', type: AssetType.project }],
    cloudHome: [{ title: 'Samples', type: AssetType.directory }],
    shouldOpen: false,
  },
  {
    localHome: [{ title: 'Samples', type: AssetType.directory }],
    cloudHome: [{ title: 'Samples', type: AssetType.project }],
    shouldOpen: false,
  },
])(
  'Initial project with homes $localHome and $cloudHome',
  async ({ localHome, cloudHome, shouldOpen }) => {
    const result = await welcomeProjectPath(
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
