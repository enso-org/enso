import { createConfigStore } from '$/providers/config'
import { createQueryClient } from '$/utils/queryClient'
import { VueQueryPlugin } from '@tanstack/vue-query'
import { NetworkError } from 'enso-common/src/utilities/errors'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { createApp, nextTick } from 'vue'
import { createMemoryHistory, createRouter } from 'vue-router'

/**
 * Mount a composable with the application's own query client, whose defaults (retries,
 * `experimental_prefetchInRender`) decide how query failures surface.
 */
async function withAppQueryClient<T>(composable: () => T): Promise<T> {
  const queryClient = await createQueryClient()
  let result: { value: T } | undefined
  const app = createApp({
    setup() {
      result = { value: composable() }
      return () => {}
    },
  })
  app.use(VueQueryPlugin, { queryClient })
  app.mount(document.createElement('div'))
  return result!.value
}

function stubFetch(response: () => Promise<unknown>) {
  const fetch = vi.fn(response)
  vi.stubGlobal('fetch', fetch)
  return fetch
}

describe('config store with an unresolvable Cloud host', () => {
  afterEach(() => {
    vi.unstubAllGlobals()
  })

  it('reports the Cloud as unreachable', async () => {
    stubFetch(() => Promise.reject(new TypeError('Failed to fetch')))
    const config = await withAppQueryClient(() => createConfigStore())

    await config.waitForRemoteConfig()

    expect(config.isCloudUnreachable).toBe(true)
    expect(config.remoteConfig).toBeUndefined()
  })

  it('does not retry a host that cannot be reached', async () => {
    const fetch = stubFetch(() => Promise.reject(new NetworkError('Failed to fetch')))
    const config = await withAppQueryClient(() => createConfigStore())

    await config.waitForRemoteConfig()

    expect(fetch).toHaveBeenCalledTimes(1)
  })

  it('still retries a host that answers with a failure', async () => {
    const fetch = stubFetch(() => Promise.resolve({ ok: false, status: 500 }))
    const config = await withAppQueryClient(() => createConfigStore())

    await vi.waitFor(() => expect(fetch.mock.calls.length).toBeGreaterThan(1), { timeout: 5_000 })
    // A reachable host that merely failed is not a reachability problem.
    expect(config.isCloudUnreachable).toBe(false)
  })

  it('lets navigation complete instead of stranding the app on the loading screen', async () => {
    stubFetch(() => Promise.reject(new TypeError('Failed to fetch')))
    const config = await withAppQueryClient(() => createConfigStore())

    const router = createRouter({
      history: createMemoryHistory(),
      routes: [{ path: '/', name: 'dashboard', component: { template: '<div />' } }],
    })
    const errors: unknown[] = []
    router.onError((error) => errors.push(error))
    router.beforeEach(async () => {
      await config.waitForRemoteConfig()
    })

    await router.push('/').catch((error: unknown) => errors.push(error))
    await nextTick()

    expect(errors).toEqual([])
    expect(router.currentRoute.value.name).toBe('dashboard')
    expect(router.currentRoute.value.matched.length).toBeGreaterThan(0)
  })
})
