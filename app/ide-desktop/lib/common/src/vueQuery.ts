/** @file QueryClient based on the '@tanstack/vue-query' implementation. */

import * as vueQuery from '@tanstack/vue-query'
import * as queryCore from '@tanstack/query-core'
import * as vue from 'vue'

/** The QueryClient from vue-query, but with immediate query invalidation. */
export class QueryClient extends vueQuery.QueryClient {
    /** Like the `invalidateQueries` method of `vueQuery.QueryClient`, but invalidates queries immediately. */
    // Workaround for https://github.com/TanStack/query/issues/7694
    override invalidateQueries(
        filters: MaybeRefDeep<queryCore.InvalidateQueryFilters> = {},
        options: MaybeRefDeep<queryCore.InvalidateOptions> = {}
    ): Promise<void> {
        const filtersValue = cloneDeepUnref(filters)
        const optionsValue = cloneDeepUnref(options)
        queryCore.notifyManager.batch(() => {
            this.getQueryCache()
                .findAll(filtersValue)
                .forEach(query => {
                    query.invalidate()
                })
        })
        if (filtersValue.refetchType === 'none') {
            return Promise.resolve()
        } else {
          const refetchType = filtersValue.refetchType
          return vue.nextTick(() =>
            queryCore.notifyManager.batch(() => {
              const refetchFilters: queryCore.RefetchQueryFilters = {
                ...filtersValue,
                type: refetchType ?? filtersValue.type ?? 'active',
              }
              return this.refetchQueries(refetchFilters, optionsValue)
            })
          )
        }
    }
}

/* eslint-disable */

function isPlainObject(value: unknown): value is Object {
    if (Object.prototype.toString.call(value) !== '[object Object]') {
        return false
    }

    const prototype = Object.getPrototypeOf(value)
    return prototype === null || prototype === Object.prototype
}

function cloneDeep<T>(
    value: MaybeRefDeep<T>,
    customize?: (val: MaybeRefDeep<T>) => T | undefined
): T {
    if (customize) {
        const result = customize(value)
        // If it's a ref of undefined, return undefined
        if (result === undefined && vue.isRef(value)) {
            return result as T
        }
        if (result !== undefined) {
            return result
        }
    }

    if (Array.isArray(value)) {
        return value.map(val => cloneDeep(val, customize)) as unknown as T
    }

    if (typeof value === 'object' && isPlainObject(value)) {
        const entries = Object.entries(value).map(([key, val]) => [key, cloneDeep(val, customize)])
        return Object.fromEntries(entries)
    }

    return value as T
}

function cloneDeepUnref<T>(obj: MaybeRefDeep<T>): T {
    return cloneDeep(obj, val => {
        if (vue.isRef(val)) {
            return cloneDeepUnref(vue.unref(val))
        }

        return undefined
    })
}

type MaybeRefDeep<T> = vue.MaybeRef<
    T extends Function
        ? T
        : T extends object
          ? {
                [Property in keyof T]: MaybeRefDeep<T[Property]>
            }
          : T
>

/* eslint-enable */
