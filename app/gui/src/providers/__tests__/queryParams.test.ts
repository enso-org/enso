import { withSetup } from '@/util/testing'
import { describe, expect, test, vi } from 'vitest'
import { nextTick, reactive } from 'vue'
import { LocationQuery, parseQuery } from 'vue-router'
import { createQueryParams } from '../queryParams'

describe('QueryParams store', () => {
  function useFixture(
    query: string,
    expectedNavigation?: { type: 'push' | 'replace'; query: LocationQuery },
  ) {
    function checkNavigationParam(type: 'push' | 'replace') {
      return (param: { query: LocationQuery }) => {
        if (expectedNavigation?.type === type) expect(param.query).toEqual(expectedNavigation.query)
        return Promise.resolve()
      }
    }

    const router = {
      push: vi.fn(checkNavigationParam('push')),
      replace: vi.fn(checkNavigationParam('replace')),
    }
    const route = reactive({ query: parseQuery(query) })
    const queryParams = createQueryParams(router, route)

    function expectNoNavigation() {
      expect(router.push).not.toHaveBeenCalled()
      expect(router.replace).not.toHaveBeenCalled()
    }

    function expectNavigation() {
      expect(router.push).toHaveBeenCalledTimes(expectedNavigation?.type === 'push' ? 1 : 0)
      expect(router.replace).toHaveBeenCalledTimes(expectedNavigation?.type === 'replace' ? 1 : 0)
    }

    return { queryParams, expectNavigation, expectNoNavigation, route }
  }

  test.each`
    query                      | expectedFoo  | expectedBar  | expectedBaz
    ${''}                      | ${undefined} | ${undefined} | ${undefined}
    ${'foo=1&bar=two'}         | ${'1'}       | ${'two'}     | ${undefined}
    ${'foo=1&bar=two&foo=one'} | ${'1'}       | ${'two'}     | ${undefined}
  `(
    'Read query params $query',
    ({ query, expectedFoo, expectedBar, expectedBaz }) =>
      withSetup(() => {
        const { queryParams, expectNoNavigation } = useFixture(query)

        expect(queryParams.get('foo')).toBe(expectedFoo)
        expect(queryParams.get('bar')).toBe(expectedBar)
        expect(queryParams.get('baz')).toBe(expectedBaz)
        expectNoNavigation()
      })[0],
  )

  test('Update single param', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz', {
        type: 'push',
        query: { foo: 'test', bar: 'baz' },
      })

      queryParams.set('foo', 'test')
      expect(queryParams.get('foo')).toBe('test')
      expectNoNavigation()
      await nextTick()
      expect(queryParams.get('foo')).toBe('test')
      expect(queryParams.get('bar')).toBe('baz')
      expectNavigation()
    })[0])

  test('Clear param', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz', {
        type: 'push',
        query: { bar: 'baz' },
      })

      queryParams.clear('foo')
      expect(queryParams.get('foo')).toBeUndefined()
      expectNoNavigation()
      await nextTick()
      expect(queryParams.get('foo')).toBeUndefined()
      expect(queryParams.get('bar')).toBe('baz')
      expectNavigation()
    })[0])

  test('Batch mutliple edits', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz', {
        type: 'push',
        query: { bar: 'test', baz: 'test2' },
      })
      queryParams.clear('foo')
      queryParams.set('bar', 'test')
      queryParams.set('baz', 'test2', true)
      expectNoNavigation()
      await nextTick()
      expect(queryParams.get('foo')).toBeUndefined()
      expect(queryParams.get('bar')).toBe('test')
      expect(queryParams.get('baz')).toBe('test2')
      expectNavigation()
    })[0])

  test('Mutliple edits but no change', () =>
    withSetup(async () => {
      const { queryParams, expectNoNavigation } = useFixture('foo=bar&bar=baz', {
        type: 'push',
        query: { bar: 'test', baz: 'test2' },
      })
      queryParams.clear('foo')
      queryParams.set('bar', 'test')
      queryParams.set('foo', 'bar')
      queryParams.set('bar', 'baz')
      expectNoNavigation()
      await nextTick()
      expect(queryParams.get('foo')).toBe('bar')
      expect(queryParams.get('bar')).toBe('baz')
      expectNoNavigation()
    })[0])

  test('Replace history', () =>
    withSetup(async () => {
      const { queryParams, expectNavigation, expectNoNavigation } = useFixture('foo=bar&bar=baz', {
        type: 'replace',
        query: { bar: 'test' },
      })
      queryParams.clear('foo', true)
      queryParams.set('bar', 'test', true)
      expectNoNavigation()
      await nextTick()
      expect(queryParams.get('foo')).toBeUndefined()
      expect(queryParams.get('bar')).toBe('test')
      expectNavigation()
    })[0])

  test('Handle external update', () =>
    withSetup(async () => {
      const { queryParams, expectNoNavigation, route } = useFixture('foo=bar&bar=baz')
      route.query = parseQuery('bar=baz&baz=test2')
      expectNoNavigation()
      await nextTick()
      expect(queryParams.get('foo')).toBeUndefined()
      expect(queryParams.get('bar')).toBe('baz')
      expect(queryParams.get('baz')).toBe('test2')
      expectNoNavigation()
    }))
})
