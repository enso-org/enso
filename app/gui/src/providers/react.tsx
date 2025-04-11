import { GuiConfig, injectGuiConfig } from '@/providers/guiConfig'
import { assert } from '@/util/assert'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'
import { computed } from 'vue'
import { Router, useRoute, useRouter } from 'vue-router'

function useInReactFunction<T>(context: react.Context<T | null>) {
  return () => {
    const value = react.useContext(context)
    assert(value != null, "Context for React wasn't provided")
    return value
  }
}

interface RouterForReact {
  router: Router
  route: ReturnType<typeof useRoute>
  searchParams: URLSearchParams
}
const RouterContext = react.createContext<RouterForReact | null>(null)
export const useRouterInReact = useInReactFunction(RouterContext)

const ConfigContext = react.createContext<GuiConfig | null>(null)
export const useConfigInReact = useInReactFunction(ConfigContext)

/**
 * A provider for all contexts set in vue and read by react.
 *
 * The default "crossing providers" from veaury has some downsides, for example
 * nesting two in a row does not work.
 */
export const ContextsForReactProvider = applyPureReactInVue(
  ({
    children,
    router,
    config,
  }: react.PropsWithChildren<{ router: RouterForReact; config: GuiConfig }>) => {
    return (
      <RouterContext.Provider value={router}>
        <ConfigContext.Provider value={config}>{children}</ConfigContext.Provider>
      </RouterContext.Provider>
    )
  },
  {
    useInjectPropsFromWrapper: () => {
      const route = useRoute()
      const router = useRouter()
      return {
        router: computed(() => {
          const searchParams = computed(() => {
            const queryFlatList = Object.entries(route.query).flatMap(([key, value]) => {
              if (value instanceof Array) {
                return value.map((singleVal) => [key, singleVal ?? ''])
              } else {
                return [[key, value ?? '']]
              }
            })
            return new URLSearchParams(queryFlatList)
          })
          return {
            router,
            route,
            searchParams: searchParams.value,
          }
        }),
        config: injectGuiConfig(),
      }
    },
  },
)
