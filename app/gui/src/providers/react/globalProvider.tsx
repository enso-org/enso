import LocalStorage from '#/utilities/LocalStorage'
import { ActionsStore, useActionsStore } from '$/providers/actions'
import { AuthStore, useAuth } from '$/providers/auth'
import { BackendsStore, useBackends } from '$/providers/backends'
import { useHttpClient } from '$/providers/httpClient'
import { QueryParams, useQueryParams } from '$/providers/queryParams'
import {
  ActionsContext,
  ConfigContext,
  HTTPClientContext,
  LocalStorageContext,
  SessionContext,
  TextContext,
} from '$/providers/react'
import { AuthContext } from '$/providers/react/auth'
import { BackendsContext } from '$/providers/react/backends'
import { QueryParamsContext } from '$/providers/react/queryParams'
import { RouterContext, RouterForReact } from '$/providers/react/router'
import { SessionStore, useSession } from '$/providers/session'
import { TextStore, useText } from '$/providers/text'
import { GuiConfig, injectGuiConfig } from '@/providers/guiConfig'
import { reactComponent } from '@/util/react'
import { proxyRefs } from '@/util/reactivity'
import type { HttpClient } from 'enso-common/src/services/HttpClient'
import * as react from 'react'
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
  queryParams: QueryParams
  actionsStore: ActionsStore
}

/**
 * A provider for all global contexts set in vue and read by react.
 *
 * The default "crossing providers" from veaury has some downsides, for example
 * nesting two in a row does not work.
 */
export const ContextsForReactProvider = reactComponent(
  (props: react.PropsWithChildren<ContextsForReactProviderProps>) => {
    const {
      children,
      router,
      config,
      text,
      httpClient,
      backends,
      localStorage,
      session,
      auth,
      queryParams,
      actionsStore,
    } = props
    return (
      <RouterContext.Provider value={router}>
        <ConfigContext.Provider value={config}>
          <TextContext.Provider value={text}>
            <HTTPClientContext.Provider value={httpClient}>
              <LocalStorageContext.Provider value={localStorage}>
                <SessionContext.Provider value={session}>
                  <AuthContext.Provider value={auth}>
                    <QueryParamsContext.Provider value={queryParams}>
                      <BackendsContext.Provider value={backends}>
                        <ActionsContext.Provider value={actionsStore}>
                          {children}
                        </ActionsContext.Provider>
                      </BackendsContext.Provider>
                    </QueryParamsContext.Provider>
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
      const result = proxyRefs({
        router: {
          router,
          route,
        },
        config: injectGuiConfig(),
        text: useText(),
        httpClient: useHttpClient(),
        backends: useBackends(),
        localStorage: LocalStorage.getInstance(),
        session: useSession(),
        auth: useAuth(),
        queryParams: useQueryParams(),
        actionsStore: useActionsStore(),
      })
      // Avoid annoying warning about __veauryInjectedProps__ property. Returning a function here
      // avoids the code path that assigns that property to overwrite a computed value with constant.
      return () => result
    },
  },
) as any
