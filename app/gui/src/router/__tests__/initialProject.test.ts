import ProjectProvider from '#/providers/ProjectsProvider'
import { EmailAddress, OrganizationId, Path, Plan, User, UserId } from '#/services/Backend'
import { newDirectoryId } from '#/services/LocalBackend'
import { expect, test, vi } from 'vitest'
import {
  CLOUD_INITIAL_PROJECT_RELATIVE_PATH,
  initialProjectPath,
  LOCAL_INITIAL_PROJECT_RELATIVE_PATH,
} from '../initialProject'

// keep import registerting local storage key
const _ = ProjectProvider
const CLOUD_ROOT_PATH = 'enso://Users/user'
const LOCAL_ROOT_PATH = '/home/user/Documents/enso-projects'
const USER: User = {
  isEnabled: true,
  isOrganizationAdmin: false,
  rootDirectoryId: newDirectoryId(Path(CLOUD_ROOT_PATH)),
  userGroups: null,
  plan: Plan.free,
  isEnsoTeamMember: false,
  organizationId: OrganizationId('organization-Mock'),
  userId: UserId('Mock user'),
  name: 'mock',
  email: EmailAddress('mock@mock.com'),
}

function mockBackends() {
  return {
    localBackend: { rootPath: vi.fn(() => LOCAL_ROOT_PATH) },
    remoteBackend: { rootPath: vi.fn(() => CLOUD_ROOT_PATH) },
  }
}

test.each([Plan.free, Plan.solo, Plan.team, Plan.enterprise])(
  'Initial project from configuration with %s plan',
  (plan) => {
    const resultFromName = initialProjectPath('Name', { ...USER, plan }, mockBackends())
    expect(resultFromName).toBe('local//home/user/Documents/enso-projects/Name')
    const resultFromURL = initialProjectPath(
      'file:///home/user/Name.enso-project',
      { ...USER, plan },
      mockBackends(),
    )
    expect(resultFromURL).toBeUndefined()
  },
)

test.each`
  plan               | expected
  ${Plan.free}       | ${`local//home/user/Documents/enso-projects/${LOCAL_INITIAL_PROJECT_RELATIVE_PATH}`}
  ${Plan.solo}       | ${`cloud/Users/user/${CLOUD_INITIAL_PROJECT_RELATIVE_PATH}`}
  ${Plan.team}       | ${`cloud/Users/user/${CLOUD_INITIAL_PROJECT_RELATIVE_PATH}`}
  ${Plan.enterprise} | ${`cloud/Users/user/${CLOUD_INITIAL_PROJECT_RELATIVE_PATH}`}
`('Initial project on fresh install with $plan plan', ({ plan, expected }) => {
  const result = initialProjectPath(undefined, { ...USER, plan }, mockBackends())
  expect(result).toBe(expected)
})
