import { withSetup } from '@/util/testing'
import { describe, expect, test, vi } from 'vitest'
import { nextTick, reactive } from 'vue'
import { parseQuery, type LocationQuery } from 'vue-router'
import { createQueryParams } from '../queryParams'

describe('QueryParams store', () => {
  function useFixture(query: string) {
    const expectedQuery = {
      push: undefined as LocationQuery | undefined,
      replace: undefined as LocationQuery | undefined,
    }
    function checkNavigationParam(type: keyof typeof expectedQuery) {
      return (param: { query: LocationQuery }) => {
        expect(param.query).toEqual(expectedQuery[type])
        route.query = param.query
        return Promise.resolve()
      }
    }
    const router = {
      push: vi.fn(checkNavigationParam('push')),
      replace: vi.fn(checkNavigationParam('replace')),
    }
    const route = reactive({ query: parseQuery(query) })
    const queryParams = createQueryParams(router, route)

    function expectNoNavigation<T>(f: () => T): T {
      router.push.mockClear()
      router.replace.mockClear()

      const result = f()

      const check = () => {
        expect(router.push).not.toHaveBeenCalled()
        expect(router.replace).not.toHaveBeenCalled()
      }
      if (result instanceof Promise) return result.then((val) => (check(), val)) as T
      check()
      return result
    }

    function expectNavigation<T>(type: 'push' | 'replace', query: LocationQuery, f: () => T): T {
      router.push.mockClear()
      router.replace.mockClear()
      const expectedCall = router[type]
      const notExpectedCall = type === 'push' ? router.replace : router.push
      expectedQuery[type] = query

      const result = f()

      const check = () => {
        expect(expectedCall).toHaveBeenCalledOnce()
        expect(notExpectedCall).not.toHaveBeenCalled()
      }
      if (result instanceof Promise) return result.then((val) => (check(), val)) as T
      check()
      return result
    }

    return { queryParams, expectNavigation, expectNoNavigation, route }
  }

  test.each`
    query                      | expectedFoo  | expectedBar
    ${''}                      | ${undefined} | ${undefined}
    ${'foo=1&bar=two'}         | ${'1'}       | ${'two'}
    ${'foo=1&bar=two&foo=one'} | ${'1'}       | ${'two'}
  `('Read query params $query', ({ query, expectedFoo, expectedBar }) =>
    withSetup(() => {
      const { queryParams } = useFixture(query)

      expect(queryParams.get('foo')).toBe(expectedFoo)
      expect(queryParams.get('bar')).toBe(expectedBar)
      expect(queryParams.get('baz')).toBe(undefined)
    }),
  )

  test('Update single param', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz')

      expectNoNavigation(() => queryParams.set('foo', 'test'))
      expect(queryParams.get('foo')).toBe('test')
      await expectNavigation('push', { foo: 'test', bar: 'baz' }, nextTick)
      expect(queryParams.get('foo')).toBe('test')
      expect(queryParams.get('bar')).toBe('baz')
    }))

  test('Deduplicate on update param', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture(
        'foo=bar&bar=baz&foo=1&bar=two',
      )

      expectNoNavigation(() => queryParams.set('foo', 'test'))
      await expectNavigation('push', { foo: 'test', bar: 'baz' }, nextTick)
      expect(queryParams.get('foo')).toBe('test')
      expect(queryParams.get('bar')).toBe('baz')
    }))

  test('Clear param', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz')

      expectNoNavigation(() => queryParams.clear('foo'))
      expect(queryParams.get('foo')).toBeUndefined()
      await expectNavigation('push', { bar: 'baz' }, nextTick)
      expect(queryParams.get('foo')).toBeUndefined()
      expect(queryParams.get('bar')).toBe('baz')
    }))

  test('Batch mutliple edits', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz')
      expectNoNavigation(() => {
        queryParams.clear('foo')
        queryParams.set('bar', 'test')
        queryParams.set('baz', 'test2')
      })
      await expectNavigation('push', { bar: 'test', baz: 'test2' }, nextTick)
      expect(queryParams.get('foo')).toBeUndefined()
      expect(queryParams.get('bar')).toBe('test')
      expect(queryParams.get('baz')).toBe('test2')
    }))

  test('Mutliple edits but no change', () =>
    withSetup(async () => {
      const { queryParams, expectNoNavigation } = useFixture('foo=bar&bar=baz')
      expectNoNavigation(() => {
        queryParams.clear('foo')
        queryParams.set('bar', 'test')
        queryParams.set('foo', 'bar')
        queryParams.set('bar', 'baz')
        return nextTick()
      })
      expect(queryParams.get('foo')).toBe('bar')
      expect(queryParams.get('bar')).toBe('baz')
    }))

  test('Replace history', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz')

      // Because implementation involves a global flag, run two rounds of different navigations
      // to make sure it is cleared properly.
      const RUNS = 2
      for (let i = 0; i < RUNS; ++i) {
        // Replace
        expectNoNavigation(() => {
          queryParams.clear('foo', true)
          queryParams.set('bar', 'test', true)
        })
        await expectNavigation('replace', { bar: 'test' }, nextTick)
        expect(queryParams.get('foo')).toBeUndefined()
        expect(queryParams.get('bar')).toBe('test')

        // Push
        expectNoNavigation(() => {
          queryParams.set('baz', 'test2')
        })
        await expectNavigation('push', { bar: 'test', baz: 'test2' }, nextTick)

        // Mixed
        expectNoNavigation(() => {
          queryParams.clear('baz', true)
          queryParams.set('foo', 'bar')
          queryParams.set('bar', 'baz', true)
        })
        await expectNavigation('push', { foo: 'bar', bar: 'baz' }, nextTick)
      }
    }))

  test.each`
    query                                | expectedFoo  | expectedBar
    ${''}                                | ${undefined} | ${undefined}
    ${'foo=1&bar=two'}                   | ${'1'}       | ${'two'}
    ${'bar=two'}                         | ${undefined} | ${'two'}
    ${'foo=bar&bar=baz'}                 | ${'bar'}     | ${'baz'}
    ${'foo=bar&bar=two'}                 | ${'bar'}     | ${'two'}
    ${'foo=bar&bar=baz&foo=bar&bar=baz'} | ${'bar'}     | ${'baz'}
  `('Handle external update $query', ({ query, expectedFoo, expectedBar }) =>
    withSetup(async () => {
      const { queryParams, expectNoNavigation, route } = useFixture('foo=bar&bar=baz')

      await expectNoNavigation(async () => {
        route.query = parseQuery(query)
        await nextTick()
      })

      expect(queryParams.get('foo')).toBe(expectedFoo)
      expect(queryParams.get('bar')).toBe(expectedBar)
      expect(queryParams.get('baz')).toBe(undefined)
    }),
  )
})
