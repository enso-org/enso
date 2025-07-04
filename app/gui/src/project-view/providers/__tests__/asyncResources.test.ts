import { provideOpenedProjects } from '$/providers/openedProjects'
import { withSetup } from '@/util/testing'
import { beforeEach } from 'node:test'
import { describe, expect, test } from 'vitest'
import { AsyncResourceStore, provideAsyncResources } from '../asyncResources'

describe('asyncResources', () => {
  function withAsyncResources(fn: (res: AsyncResourceStore) => void) {
    return async () => {
      await withSetup(() => {
        const openedProjects = provideOpenedProjects()
        const res = provideAsyncResources(openedProjects)
        return fn(res)
      })
    }
  }

  beforeEach(() => {
    const openedProjects = provideOpenedProjects()
    provideAsyncResources(openedProjects)
  })
  test(
    'provider works',
    withAsyncResources((res) => {
      expect(res).toBeDefined()
    }),
  )
})
