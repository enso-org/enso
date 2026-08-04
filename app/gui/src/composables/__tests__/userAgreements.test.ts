import LocalStorage from '#/utilities/LocalStorage'
import { useUserAgreements } from '$/composables/userAgreements'
import * as vueQuery from '@tanstack/vue-query'
import { VueQueryPlugin, type QueryClient } from '@tanstack/vue-query'
import { afterEach, beforeEach, describe, expect, it, vi } from 'vitest'
import { createApp } from 'vue'

/** Mount a composable, giving it the query client it needs. */
async function withSetup<T>(composable: (queryClient: QueryClient) => Promise<T>): Promise<T> {
  let result: { value: Promise<T> } | undefined
  const app = createApp({
    setup() {
      const queryClient = vueQuery.useQueryClient()
      result = { value: composable(queryClient) }
      return () => {}
    },
  })
  app.use(VueQueryPlugin)
  app.mount(document.createElement('div'))
  return result!.value
}

const TOS_HASH = 'tos-hash'
const PRIVACY_HASH = 'privacy-hash'

function stubReachableDocuments() {
  vi.stubGlobal(
    'fetch',
    vi.fn((url: URL) => {
      const hash = String(url).includes('eula') ? TOS_HASH : PRIVACY_HASH
      return Promise.resolve({ ok: true, json: () => Promise.resolve({ hash }) })
    }),
  )
}

function stubUnreachableDocuments() {
  vi.stubGlobal(
    'fetch',
    vi.fn(() => Promise.reject(new TypeError('Failed to fetch'))),
  )
}

describe('useUserAgreements', () => {
  beforeEach(() => {
    window.localStorage.clear()
    LocalStorage.getInstance().delete('termsOfService')
    LocalStorage.getInstance().delete('privacyPolicy')
  })

  afterEach(() => {
    vi.unstubAllGlobals()
  })

  it('treats unreadable documents as not agreed to, so the Cloud stays blocked', async () => {
    stubUnreachableDocuments()

    const agreements = await withSetup(useUserAgreements)

    expect(agreements.agreedToTos).toBe(false)
    expect(agreements.agreedToPrivacyPolicy).toBe(false)
  })

  it('records nothing when the accepted version is unknown', async () => {
    stubUnreachableDocuments()

    const agreements = await withSetup(useUserAgreements)
    agreements.userAgreed()

    // Accepting an agreement whose version could not be read must not mark it as accepted.
    expect(LocalStorage.getInstance().get('termsOfService')).toBeUndefined()
    expect(agreements.agreedToTos).toBe(false)
  })

  it('asks for agreement when the documents are readable but nothing was accepted', async () => {
    stubReachableDocuments()

    const agreements = await withSetup(useUserAgreements)

    expect(agreements.agreedToTos).toBe(false)
    expect(agreements.agreedToPrivacyPolicy).toBe(false)
  })

  it('accepts the current versions once the documents are readable', async () => {
    stubReachableDocuments()

    const agreements = await withSetup(useUserAgreements)
    agreements.userAgreed()

    expect(LocalStorage.getInstance().get('termsOfService')).toEqual({ versionHash: TOS_HASH })
    expect(agreements.agreedToTos).toBe(true)
    expect(agreements.agreedToPrivacyPolicy).toBe(true)
  })
})
