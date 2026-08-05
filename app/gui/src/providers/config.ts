import { isCloudUnreachableError } from '$/utils/cloudReachability'
import { proxyRefs } from '$/utils/reactivity'
import { waitForData } from '@/util/tanstack'
import * as sentry from '@sentry/vue'
import { useQuery } from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { parseWebAppOptionsFromSearchParams } from 'enso-common/src/options'
import { CONFIGURATION_PATH } from 'enso-common/src/services/Backend/remoteBackendPaths'
import { computed, watch, watchEffect } from 'vue'
import * as z from 'zod'

const HTTP_STATUS_BAD_REQUEST = 400

const REMOTE_CONFIG_SCHEMA = z.object({
  ENSO_IDE_ENVIRONMENT: z.optional(z.string()),
  ENSO_IDE_API_URL: z.optional(z.string()),
  ENSO_IDE_AUTH_ENDPOINT: z.optional(z.string()),
  ENSO_IDE_STRIPE_KEY: z.optional(z.string()),
  ENSO_IDE_COGNITO_USER_POOL_ID: z.optional(z.string()),
  ENSO_IDE_COGNITO_USER_POOL_WEB_CLIENT_ID: z.optional(z.string()),
  ENSO_IDE_COGNITO_DOMAIN: z.optional(z.string()),
  ENSO_IDE_COGNITO_REGION: z.optional(z.string()),
  ENSO_IDE_GOOGLE_OAUTH_CLIENT_ID: z.optional(z.string()),
  ENSO_IDE_STRAVA_OAUTH_CLIENT_ID: z.optional(z.string()),
  ENSO_IDE_MS365_OAUTH_CLIENT_ID: z.optional(z.string()),
  ENSO_IDE_SALESFORCE_OAUTH_CLIENT_ID: z.optional(z.string()),
})

export type RemoteConfig = z.infer<typeof REMOTE_CONFIG_SCHEMA>

export type ConfigStore = ReturnType<typeof createConfigStore>

/** Create a store exposing the startup configuration, both local and fetched from the Cloud. */
export function createConfigStore() {
  const remoteConfigUrl = $config.API_URL ?? 'https://api.cloud.enso.org'

  const remoteConfig = useQuery<RemoteConfig>({
    queryKey: ['config', remoteConfigUrl],
    // Retrying an unresolvable host only delays startup — every attempt fails the same way,
    // and the whole app waits behind this query (see `waitForRemoteConfig`).
    retry: (failureCount, error) => !isCloudUnreachableError(error) && failureCount < 3,
    queryFn: async ({ queryKey: [_, url] }) => {
      const response = await fetch(`${url}/${CONFIGURATION_PATH}`)
      if (!response.ok) {
        throw new Error(`Fetch config returned ${response.status}`)
      }
      return REMOTE_CONFIG_SCHEMA.parse(await response.json())
    },
  })

  /**
   * `true` when the configuration endpoint could not be reached at all. As the endpoint is the
   * first Cloud request the app makes, this doubles as the verdict on Cloud reachability as a
   * whole: without it neither Cognito nor the Cloud API can even be addressed.
   */
  const isCloudUnreachable = computed(() => isCloudUnreachableError(remoteConfig.error.value))

  watchEffect(() => {
    if (remoteConfig.error.value != null)
      console.error('Error while fetching configuration', remoteConfig.error.value)
  })

  watch(
    () => remoteConfig.data.value?.ENSO_IDE_ENVIRONMENT,
    (env) => {
      if (env != null) console.log('Loaded config:', env)
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
    isCloudUnreachable,
    // Never rejects. This is awaited by a global navigation guard, and a guard that rejects
    // aborts the navigation for good — leaving the app on its loading screen with no route
    // component and nothing to retry it. A missing remote config is reflected in `isError` and
    // `isCloudUnreachable` instead, so the app can degrade to local projects.
    waitForRemoteConfig: () => waitForData(remoteConfig).catch(() => undefined),
  })
}

export const useConfig = createGlobalState(createConfigStore)
