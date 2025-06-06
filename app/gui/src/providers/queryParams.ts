import { createGlobalState } from '@vueuse/core'
import { reactive, ref, watch } from 'vue'
import { LocationQueryValue, useRoute, useRouter } from 'vue-router'

export type QueryParams = ReturnType<typeof createQueryParams>
function createQueryParams() {
  const router = useRouter()
  const route = useRoute()

  function getQueryValue(entry: LocationQueryValue | LocationQueryValue[] | undefined) {
    return entry instanceof Array ? entry[0] : entry
  }

  const queryParams = reactive(
    new Map(Object.entries(route.query).map(([key, value]) => [key, getQueryValue(value)])),
  )
  const anyPushed = ref(false)

  function get(key: string) {
    return queryParams.get(key)
  }
  function set(key: string, value: LocationQueryValue, replace: boolean = false) {
    queryParams.set(key, value)
    anyPushed.value ||= !replace
  }
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
    }
    for (const [key, value] of newQueryParams) {
      if (route.query[key] !== value) return navigate()
    }
    for (const key of Object.keys(route.query)) {
      if (!newQueryParams.has(key)) return navigate()
    }
  })

  return { get, set, clear }
}

export const useQueryParams = createGlobalState(createQueryParams)
