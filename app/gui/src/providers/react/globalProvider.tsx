import LocalStorage from '#/utilities/LocalStorage'
import { useActionsStore, type ActionsStore } from '$/providers/actions'
import { useAuth, type AuthStore } from '$/providers/auth'
import { useBackends, type BackendsStore } from '$/providers/backends'
import { useHttpClient } from '$/providers/httpClient'
import { useOpenedProjects, type OpenedProjectsStore } from '$/providers/openedProjects'
import { useQueryParams, type QueryParams } from '$/providers/queryParams'
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
import { OpenedProjectsContext } from '$/providers/react/openedProjects'
import { QueryParamsContext } from '$/providers/react/queryParams'
import { RouterContext, type RouterForReact } from '$/providers/react/router'
import { UploadsToCloudStoreContext } from '$/providers/react/upload'
import { useSession, type SessionStore } from '$/providers/session'
import { useText, type TextStore } from '$/providers/text'
import { useUploadsToCloudStore, type UploadsToCloudStore } from '$/providers/upload'
import { injectGuiConfig, type GuiConfig } from '@/providers/guiConfig'
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
  uploadsToCloudStore: UploadsToCloudStore
  openedProjects: OpenedProjectsStore
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
      uploadsToCloudStore,
      openedProjects,
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
                          <UploadsToCloudStoreContext.Provider value={uploadsToCloudStore}>
                            <OpenedProjectsContext.Provider value={openedProjects}>
                              {children}
                            </OpenedProjectsContext.Provider>
                          </UploadsToCloudStoreContext.Provider>
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
        uploadsToCloudStore: useUploadsToCloudStore(),
        openedProjects: useOpenedProjects(),
      })
      // Avoid annoying warning about __veauryInjectedProps__ property. Returning a function here
      // avoids the code path that assigns that property to overwrite a computed value with constant.
      return () => result
    },
  },
) as any
