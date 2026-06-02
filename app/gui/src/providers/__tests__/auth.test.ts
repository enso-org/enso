import type { UserSession as CognitoUserSession } from '$/authentication/cognito'
import { isDirectoryId, isOrganizationId, isUserId, Plan } from 'enso-common/src/services/Backend'
import { Rfc3339DateTime } from 'enso-common/src/utilities/data/dateTime'
import { describe, expect, it } from 'vitest'
import { computed } from 'vue'
import { isUsersMeQueryKey, makeSyntheticUser } from '../auth'

function fakeCognitoSession(overrides: Partial<CognitoUserSession> = {}): CognitoUserSession {
  return {
    email: 'user@example.com',
    accessToken: 'access',
    refreshToken: 'refresh',
    refreshUrl: 'https://example.com',
    expireAt: Rfc3339DateTime(new Date(Date.now() + 60_000).toJSON()),
    clientId: 'cognito-client-id',
    ...overrides,
  }
}

describe('isUsersMeQueryKey', () => {
  it('matches reactive usersMe query keys', () => {
    expect(isUsersMeQueryKey(['remote', 'usersMe', computed(() => 'client-id')])).toBe(true)
  })

  it('rejects unrelated query keys', () => {
    expect(isUsersMeQueryKey(['remote', 'otherQuery', computed(() => 'client-id')])).toBe(false)
  })
})

describe('makeSyntheticUser', () => {
  it('returns a placeholder user without any features enabled', () => {
    const user = makeSyntheticUser(fakeCognitoSession())
    expect(user.isEnabled).toBe(false)
    expect(user.isOrganizationAdmin).toBe(false)
    expect(user.isEnsoTeamMember).toBe(false)
    expect(user.plan).toBe(Plan.free)
    expect(user.userGroups).toBeNull()
    expect(user.groups).toEqual([])
  })

  it('derives identifiers in the expected newtype shape', () => {
    const user = makeSyntheticUser(fakeCognitoSession({ email: 'someone@enso.org' }))
    expect(isUserId(user.userId)).toBe(true)
    expect(user.userId).toContain('someone@enso.org')
    expect(isOrganizationId(user.organizationId)).toBe(true)
    expect(isDirectoryId(user.rootDirectoryId)).toBe(true)
  })

  it('propagates the Cognito email into name and email fields', () => {
    const user = makeSyntheticUser(fakeCognitoSession({ email: 'someone@enso.org' }))
    expect(user.email).toBe('someone@enso.org')
    expect(user.name).toBe('someone@enso.org')
  })

  it('keys identifiers on email so two users on the same Cognito app are distinct', () => {
    const a = makeSyntheticUser(fakeCognitoSession({ email: 'a@enso.org' }))
    const b = makeSyntheticUser(fakeCognitoSession({ email: 'b@enso.org' }))
    expect(a.userId).not.toBe(b.userId)
  })

  it('handles a missing email without producing an empty identifier', () => {
    const user = makeSyntheticUser(fakeCognitoSession({ email: '' }))
    expect(user.userId).toBe('user-cloud-unavailable-unknown')
  })
})
