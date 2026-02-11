import { proxyRefs } from '$/utils/reactivity'
import { waitForData } from '@/util/tanstack'
import * as sentry from '@sentry/vue'
import { useQuery } from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { parseWebAppOptionsFromSearchParams } from 'enso-common/src/options'
import { CONFIGURATION_PATH } from 'enso-common/src/services/Backend/remoteBackendPaths'
import { computed, watch } from 'vue'

const HTTP_STATUS_BAD_REQUEST = 400

export interface RemoteConfig {
  ENSO_IDE_ENVIRONMENT?: string
  ENSO_IDE_API_URL?: string
  ENSO_IDE_AUTH_ENDPOINT?: string
  ENSO_IDE_STRIPE_KEY?: string
  ENSO_IDE_COGNITO_USER_POOL_ID?: string
  ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID?: string
  ENSO_IDE_COGNITO_DOMAIN?: string
  ENSO_IDE_COGNITO_REGION?: string
  ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID?: string
  ENSO_IDE_STRAVA_OAUTH_CLIENT_ID?: string
  ENSO_IDE_MS365_OAUTH_CLIENT_ID?: string
  ENSO_IDE_SALESFORCE_OAUTH_CLIENT_ID?: string
}

export type ConfigStore = ReturnType<typeof createConfigStore>

function createConfigStore() {
  const remoteConfigUrl = $config.API_URL ?? 'https://api.cloud.enso.org'

  const remoteConfig = useQuery<RemoteConfig>({
    queryKey: ['config', remoteConfigUrl],
    queryFn: ({ queryKey: [_, url] }) =>
      fetch(`${url}/${CONFIGURATION_PATH}`).then((response) => response.json()),
  })

  watch(
    () => remoteConfig.data.value?.ENSO_IDE_ENVIRONMENT,
    (env) => {
      console.log('Loaded config:', env)
    },
    { flush: 'sync', immediate: true },
  )

  watch(
    () => remoteConfig.data.value?.ENSO_IDE_API_URL,
    (apiUrl) => {
      const sentryOptions = sentry.getClient()?.getOptions()
      if (sentryOptions != null && apiUrl != null) {
        const host = new URL(apiUrl).host
        sentryOptions.tracePropagationTargets = [apiUrl.split('//')[1] ?? '']
        sentryOptions.beforeSend = (event) => {
          if (
            (event.breadcrumbs ?? []).some(
              (breadcrumb) =>
                breadcrumb.type === 'http' &&
                breadcrumb.category === 'fetch' &&
                breadcrumb.data &&
                breadcrumb.data.status_code === HTTP_STATUS_BAD_REQUEST &&
                typeof breadcrumb.data.url === 'string' &&
                new URL(breadcrumb.data.url).host === host,
            )
          ) {
            return null
          }
          return event
        }
      }
    },
    { flush: 'sync', immediate: true },
  )

  return proxyRefs({
    params: computed(() =>
      parseWebAppOptionsFromSearchParams(new URLSearchParams(window.location.search)),
    ),
    remoteConfig: remoteConfig.data,
    isFetching: remoteConfig.isFetching,
    isError: remoteConfig.isError,
    waitForRemoteConfig: () => waitForData(remoteConfig),
  })
}

export const useConfig = createGlobalState(createConfigStore)
