import { createGlobalState } from '@vueuse/core'
import { reactive, ref, watch } from 'vue'
import { useRoute, useRouter, type LocationQueryValue, type Router } from 'vue-router'

/** Query Params store. See {@link useQueryParams}. */
export type QueryParams = ReturnType<typeof createQueryParams>

/** Create Query Params store. See {@link useQueryParams}. */
export function createQueryParams(
  router: Pick<Router, 'push' | 'replace'> = useRouter(),
  route: Pick<ReturnType<typeof useRoute>, 'query'> = useRoute(),
) {
  function getQueryValue(entry: LocationQueryValue | LocationQueryValue[] | undefined) {
    return entry instanceof Array ? entry[0] : entry
  }

  const queryParams = reactive(
    new Map(Object.entries(route.query).map(([key, value]) => [key, getQueryValue(value)])),
  )
  const anyPushed = ref(false)

  /** Read query value. Duplicated keys are discarded. */
  function get(key: string) {
    return queryParams.get(key)
  }
  /**
   * Set a single query parameter. Navigation is triggered in watch.
   * @param replace replace history entry instead of pushing new one. It will have effect only
   * if all query sets have set it to true since last navigation.
   */
  function set(key: string, value: LocationQueryValue, replace: boolean = false) {
    queryParams.set(key, value)
    anyPushed.value ||= !replace
  }
  /** Remove parameter from query. Similar to `set`. */
  function clear(key: string, replace: boolean = false) {
    queryParams.delete(key)
    anyPushed.value ||= !replace
  }

  watch(
    () => route.query,
    (newQuery) => {
      for (const key of queryParams.keys()) {
        if (!(key in newQuery)) queryParams.delete(key)
      }
      for (const key in newQuery) {
        const newVal = getQueryValue(newQuery[key])
        if (newVal !== queryParams.get(key)) {
          queryParams.set(key, newVal)
        }
      }
    },
  )

  watch(queryParams, (newQueryParams) => {
    const navigate = () => {
      const query = Object.fromEntries(newQueryParams.entries())
      if (anyPushed.value) {
        router.push({ query })
      } else {
        router.replace({ query })
      }
      anyPushed.value = false
    }
    for (const [key, value] of newQueryParams) {
      // During update we remove duplicated param keys (as we don't read them anyway).
      // Therefore we don't call `getQueryValue` here.
      if (route.query[key] !== value) return navigate()
    }
    for (const key of Object.keys(route.query)) {
      if (!newQueryParams.has(key)) return navigate()
    }
  })

  return { get, set, clear }
}

/**
 * Query Params kept and updated as map.
 *
 * This store punblish map-like API for managing single query parameters. It ensures that
 * independent query param sets in a single job won't override each other.
 */
export const useQueryParams = createGlobalState(createQueryParams)
