import HttpClient from '#/utilities/HttpClient'
import { useBackends as useBackendsVue, type BackendsStore } from '$/providers/backends'
import { GuiConfig, injectGuiConfig } from '@/providers/guiConfig'
import { assert } from '@/util/assert'
import * as react from 'react'
import { applyPureReactInVue, createCrossingProviderForPureReactInVue } from 'veaury'
import { computed, toRefs } from 'vue'
import { Router, useRoute, useRouter as useRouterVue } from 'vue-router'
import { useHttpClient as useHttpClientVue } from './httpClient'
import { RightPanelData, useRightPanelData as useRightPanelDataVue } from './rightPanel'
import { createTextStore as useTextVue, type TextStore } from './text'

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
export const useRouter = useInReactFunction(RouterContext)

const ConfigContext = react.createContext<GuiConfig | null>(null)
export const useConfig = useInReactFunction(ConfigContext)

export const TextContext = react.createContext<TextStore | null>(null)
export const useText = useInReactFunction(TextContext)

export const HTTPClientContext = react.createContext<HttpClient | null>(null)
export const useHttpClient = useInReactFunction(HTTPClientContext)

const BackendsContext = react.createContext<BackendsStore | null>(null)
export const useBackends = useInReactFunction(BackendsContext)

interface ContextsForReactProviderProps {
  router: RouterForReact
  config: GuiConfig
  text: TextStore
  httpClient: HttpClient
  backends: BackendsStore
}

/**
 * A provider for all global contexts set in vue and read by react.
 *
 * The default "crossing providers" from veaury has some downsides, for example
 * nesting two in a row does not work.
 */
export const ContextsForReactProvider = applyPureReactInVue(
  (props: react.PropsWithChildren<ContextsForReactProviderProps>) => {
    const { children, router, config, text, httpClient, backends } = props
    return (
      <RouterContext.Provider value={router}>
        <ConfigContext.Provider value={config}>
          <TextContext.Provider value={text}>
            <HTTPClientContext.Provider value={httpClient}>
              <BackendsContext.Provider value={backends}>{children}</BackendsContext.Provider>
            </HTTPClientContext.Provider>
          </TextContext.Provider>
        </ConfigContext.Provider>
      </RouterContext.Provider>
    )
  },
  {
    useInjectPropsFromWrapper: () => {
      const route = useRoute()
      const router = useRouterVue()
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
        text: useTextVue(),
        httpClient: useHttpClientVue(),
        backends: useBackendsVue(),
      }
    },
  },
)

const [useRightPanelDataUntyped, RightPanelDataProviderForReact] =
  createCrossingProviderForPureReactInVue(() => toRefs(useRightPanelDataVue()))

export { RightPanelDataProviderForReact }
export const useRightPanelData = useRightPanelDataUntyped as () => RightPanelData
