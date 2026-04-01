import { useAuth } from '$/providers/auth'
import { useBackends } from '$/providers/backends'
import { proxyRefs } from '$/utils/reactivity'
import * as vueQuery from '@tanstack/vue-query'
import { BackendType } from 'enso-common/src/services/Backend'
import { computed, effectScope } from 'vue'
import * as z from 'zod'

const TEN_MINUTES_MS = 600_000
const TOS_ENDPOINT_SCHEMA = z.object({ hash: z.string() })
const PRIVACY_POLICY_ENDPOINT_SCHEMA = z.object({ hash: z.string() })

export const latestTermsOfServiceQueryOptions = vueQuery.queryOptions({
  queryKey: ['termsOfService', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/eula.json', $config.HOST))
    if (!response.ok) {
      throw new Error('Failed to fetch Terms of Service')
    } else {
      return TOS_ENDPOINT_SCHEMA.parse(await response.json())
    }
  },
  select: (data) => data.hash,
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  refetchInterval: TEN_MINUTES_MS,
})

export const latestPrivacyPolicyQueryOptions = vueQuery.queryOptions({
  queryKey: ['privacyPolicy', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/privacy.json', $config.HOST))
    if (!response.ok) {
      throw new Error('Failed to fetch Privacy Policy')
    } else {
      return PRIVACY_POLICY_ENDPOINT_SCHEMA.parse(await response.json())
    }
  },
  select: (data) => data.hash,
  refetchOnWindowFocus: true,
  refetchIntervalInBackground: true,
  refetchInterval: TEN_MINUTES_MS,
})

/**
 * Composable checking and setting user agreements to the newest Terms of Service
 * and Privacy Policy.
 */
export async function useUserAgreements(queryClient: vueQuery.QueryClient) {
  const { remoteBackend } = useBackends()
  const auth = useAuth()

  const cachedTosHash = computed(() => ({ versionHash: auth.session?.user?.tosAccepted }))
  const cachedPrivacyPolicyHash = computed(() => ({ versionHash: auth.session?.user?.ppAccepted }))

  // a scope to run after await -
  const scope = effectScope()
  const initialTosHash =
    cachedTosHash.value?.versionHash ??
    (await queryClient.fetchQuery(latestTermsOfServiceQueryOptions)).hash
  const initialPrivacyPolicyHash =
    cachedPrivacyPolicyHash.value?.versionHash ??
    (await queryClient.fetchQuery(latestPrivacyPolicyQueryOptions)).hash

  return scope.run(() => {
    const { data: tosHash } = vueQuery.useQuery(
      { ...latestTermsOfServiceQueryOptions, initialData: { hash: initialTosHash } },
      queryClient,
    )
    const { data: privacyPolicyHash } = vueQuery.useQuery(
      { ...latestPrivacyPolicyQueryOptions, initialData: { hash: initialPrivacyPolicyHash } },
      queryClient,
    )

    const agreedToTos = computed(() => tosHash.value === cachedTosHash.value?.versionHash)
    const agreedToPrivacyPolicy = computed(
      () => privacyPolicyHash.value === cachedPrivacyPolicyHash.value?.versionHash,
    )
    const userAgreed = async () => {
      await remoteBackend
        .updateUser({ tosAccepted: tosHash.value, ppAccepted: privacyPolicyHash.value })
        .then(async () => {
          await queryClient.invalidateQueries({
            queryKey: [BackendType.remote, 'usersMe'],
          })
        })
    }

    return proxyRefs({
      agreedToTos,
      agreedToPrivacyPolicy,
      userAgreed,
    })
  })!
}
