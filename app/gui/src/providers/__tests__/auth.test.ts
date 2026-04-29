import { describe, expect, it } from 'vitest'
import { computed } from 'vue'
import { isUsersMeQueryKey } from '../auth'

describe('isUsersMeQueryKey', () => {
  it('matches reactive usersMe query keys', () => {
    expect(isUsersMeQueryKey(['remote', 'usersMe', computed(() => 'client-id')])).toBe(true)
  })

  it('rejects unrelated query keys', () => {
    expect(isUsersMeQueryKey(['remote', 'otherQuery', computed(() => 'client-id')])).toBe(false)
  })
})
