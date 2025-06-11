import HttpClient from '#/utilities/HttpClient'
import LocalStorage from '#/utilities/LocalStorage'
import { AuthStore, useAuth } from '$/providers/auth'
import { BackendsStore, useBackends } from '$/providers/backends'
import { useHttpClient } from '$/providers/httpClient'
import {
  BackendsContext,
  ConfigContext,
  HTTPClientContext,
  LocalStorageContext,
  SessionContext,
  TextContext,
} from '$/providers/react'
import { AuthContext } from '$/providers/react/auth'
import { RouterContext, RouterForReact } from '$/providers/react/router'
import { SessionStore, useSession } from '$/providers/session'
import { TextStore, useText } from '$/providers/text'
import { GuiConfig, injectGuiConfig } from '@/providers/guiConfig'
import * as react from 'react'
import { applyPureReactInVue } from 'veaury'
import { computed } from 'vue'
import { useRoute, useRouter } from 'vue-router'

interface ContextsForReactProviderProps {
  router: RouterForReact
  config: GuiConfig
  text: TextStore
  httpClient: HttpClient
  backends: BackendsStore
  localStorage: LocalStorage
  session: SessionStore
  auth: AuthStore
}

/**
 * A provider for all global contexts set in vue and read by react.
 *
 * The default "crossing providers" from veaury has some downsides, for example
 * nesting two in a row does not work.
 */
export const ContextsForReactProvider = applyPureReactInVue(
  (props: react.PropsWithChildren<ContextsForReactProviderProps>) => {
    const { children, router, config, text, httpClient, backends, localStorage, session, auth } =
      props
    return (
      <RouterContext.Provider value={router}>
        <ConfigContext.Provider value={config}>
          <TextContext.Provider value={text}>
            <HTTPClientContext.Provider value={httpClient}>
              <LocalStorageContext.Provider value={localStorage}>
                <SessionContext.Provider value={session}>
                  <AuthContext.Provider value={auth}>
                    <BackendsContext.Provider value={backends}>{children}</BackendsContext.Provider>
                  </AuthContext.Provider>
                </SessionContext.Provider>
              </LocalStorageContext.Provider>
            </HTTPClientContext.Provider>
          </TextContext.Provider>
        </ConfigContext.Provider>
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
        text: useText(),
        httpClient: useHttpClient(),
        backends: useBackends(),
        localStorage: LocalStorage.getInstance(),
        session: useSession(),
        auth: useAuth(),
      }
    },
  },
)
