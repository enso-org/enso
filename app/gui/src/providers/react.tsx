import { GuiConfig, injectGuiConfig } from '@/providers/guiConfig'
import { assert } from '@/util/assert'
import * as react from 'react'
import {
  allModeReturn,
  applyPureReactInVue,
  createCrossingProviderForPureReactInVue,
  ReactContext,
  VueComponent,
} from 'veaury'
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

export const ContextsForReactProvider = applyPureReactInVue(
  ({
    children,
    router,
    config,
  }: react.PropsWithChildren<{ router: RouterForReact; config: GuiConfig }>) => {
    console.log('ROUTER', router)
    console.log('CONFIG', config)
    return (
      <RouterContext.Provider value={router}>
        <ConfigContext.Provider value={config}>{children}</ConfigContext.Provider>
      </RouterContext.Provider>
    )
  },
  {
    useInjectPropsFromWrapper: () => {
      console.log('Inject props is run')
      const route = useRoute()
      const router = useRouter()
      return {
        router: computed(() => {
          console.log('Recomputed router')
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

/**
 * A wrapper for {@link createCrossingProviderForPureReactInVue}, because veaury is very bad at
 * types.
 */
export function createContextForReact<T extends allModeReturn>(
  constructor: () => T,
): [useInReact: () => T, Provider: VueComponent, ReactContext] {
  return createCrossingProviderForPureReactInVue(constructor) as any
}
