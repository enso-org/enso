import { proxyRefs } from '$/utils/reactivity'
import { waitForData } from '@/util/tanstack'
import { useQuery } from '@tanstack/vue-query'
import { createGlobalState } from '@vueuse/core'
import { parseWebAppOptionsFromSearchParams } from 'enso-common/src/options'
import { computed, watch } from 'vue'

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
      fetch(`${url}/utils/config`).then((response) => response.json()),
  })

  watch(
    () => remoteConfig.data.value?.ENSO_IDE_ENVIRONMENT,
    (env) => {
      console.log('Loaded config:', env)
    },
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
