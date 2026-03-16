import { flushPromises } from '@vue/test-utils'
import { afterEach, describe, expect, it, vi } from 'vitest'
import { createMemoryHistory, createRouter } from 'vue-router'
import { createProtectedLayoutRedirectController } from '../protectedLayoutRedirect'

function createDeferred<T>() {
  let resolve: (value: T) => void = () => undefined
  let reject: (reason: unknown) => void = () => undefined
  const promise = new Promise<T>((promiseResolve, promiseReject) => {
    resolve = promiseResolve
    reject = promiseReject
  })
  return { promise, resolve, reject }
}

describe('createProtectedLayoutRedirectController', () => {
  afterEach(() => {
    vi.restoreAllMocks()
  })

  it('keeps the latest redirect when an in-flight navigation is cancelled', async () => {
    const blockedDashboardNavigation = createDeferred<void>()
    const router = createRouter({
      history: createMemoryHistory(),
      routes: [
        { path: '/login', component: { template: '<div />' } },
        { path: '/dashboard', component: { template: '<div />' } },
      ],
    })

    router.beforeEach(async (to) => {
      if (to.path === '/dashboard') {
        await blockedDashboardNavigation.promise
      }
    })

    await router.push('/login')

    const onError = vi.fn()
    const { redirectTo } = createProtectedLayoutRedirectController(router, onError)

    redirectTo({ path: '/dashboard' })
    await Promise.resolve()

    redirectTo({ path: '/login' })
    blockedDashboardNavigation.resolve(undefined)

    await flushPromises()

    expect(router.currentRoute.value.path).toBe('/login')
    expect(onError).not.toHaveBeenCalled()
  })
})
