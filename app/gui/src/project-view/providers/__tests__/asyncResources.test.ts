import { createOpenedProjectsStore } from '$/providers/openedProjects'
import { waitFor, withSetup } from '@/util/testing'
import { assert, describe, expect, test } from 'vitest'
import { provideAsyncResources, type AsyncResourceStore } from '../asyncResources'

describe('asyncResources', () => {
  function withAsyncResources(fn: (res: AsyncResourceStore) => void | Promise<void>) {
    return withSetup(() => {
      const res = provideAsyncResources(createOpenedProjectsStore())
      return fn(res)
    })
  }

  test('Same URL resolves to same asset', () =>
    withAsyncResources(async (res) => {
      const testUrl1 = 'data:image/gif;base64,R0lGODlhAQABAAAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=='
      const testUrl2 = 'https://example.com.local/some-image.png'
      const imageRes1 = res.useResourceFromUrl(testUrl1)
      const imageRes2 = res.useResourceFromUrl(testUrl2)
      const imageRes3 = res.useResourceFromUrl(testUrl1)
      const resource1 = imageRes1.value
      const resource2 = imageRes2.value
      const resource3 = imageRes3.value
      assert(resource1.ok)
      assert(resource2.ok)
      assert(resource3.ok)

      expect(resource1.value).not.toBe(resource2.value)
      expect(resource1.value).toBe(resource3.value)

      expect(resource1.value.status).toBe('loading')
      expect(resource1.value.url).toBe(undefined)

      await waitFor(() => resource1.value.status === 'ready')
      expect(resource2.value.status).toBe('ready')
      expect(resource1.value.url).toBe(testUrl1)
      expect(resource2.value.url).toBe(testUrl2)
    }))
})
