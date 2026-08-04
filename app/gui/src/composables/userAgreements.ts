import LocalStorage from '#/utilities/LocalStorage'
import { proxyRefs } from '@/util/reactivity'
import * as vueQuery from '@tanstack/vue-query'
import { computed, effectScope } from 'vue'
import * as z from 'zod'

declare module '#/utilities/LocalStorage' {
  /** Metadata containing the version hash of the terms of service that the user has accepted. */
  interface LocalStorageData {
    readonly termsOfService: z.infer<typeof TOS_SCHEMA>
    readonly privacyPolicy: z.infer<typeof PRIVACY_POLICY_SCHEMA>
  }
}

const TEN_MINUTES_MS = 600_000
const TOS_SCHEMA = z.object({ versionHash: z.string() })
const PRIVACY_POLICY_SCHEMA = z.object({ versionHash: z.string() })
const TOS_ENDPOINT_SCHEMA = z.object({ hash: z.string() })
const PRIVACY_POLICY_ENDPOINT_SCHEMA = z.object({ hash: z.string() })

LocalStorage.registerKey('termsOfService', { schema: TOS_SCHEMA })
LocalStorage.registerKey('privacyPolicy', { schema: PRIVACY_POLICY_SCHEMA })

const latestTermsOfServiceQueryOptions = vueQuery.queryOptions({
  queryKey: ['termsOfService', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/eula.json', $config.ENSO_HOST))
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

const latestPrivacyPolicyQueryOptions = vueQuery.queryOptions({
  queryKey: ['privacyPolicy', 'currentVersion'],
  queryFn: async () => {
    const response = await fetch(new URL('/privacy.json', $config.ENSO_HOST))
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
 * Fetch the current version hash of an agreement document, or `undefined` if it cannot be
 * retrieved.
 *
 * The caller runs inside a navigation guard, and a guard that rejects aborts the navigation for
 * good — leaving the app on its loading screen. No failure to read an agreement document justifies
 * that, so failures are reported and swallowed.
 */
async function fetchAgreementHash(fetchDocument: () => Promise<{ hash: string }>) {
  try {
    return (await fetchDocument()).hash
  } catch (error) {
    console.error('Cannot read the latest user agreements; will not ask to accept them.', error)
    return undefined
  }
}

/**
 * Composable checking and setting user agreements to the newest Terms of Service
 * and Privacy Policy.
 *
 * Returns `undefined` when the agreements cannot be read and none are cached, in which case the
 * user cannot be asked to accept them. They are re-checked on every navigation and on an interval,
 * so the prompt appears once the documents become reachable again.
 */
export async function useUserAgreements(queryClient: vueQuery.QueryClient) {
  const localStorage = LocalStorage.getInstance()
  const cachedTosHash = computed(() => localStorage.get('termsOfService'))
  const cachedPrivacyPolicyHash = computed(() => localStorage.get('privacyPolicy'))

  // a scope to run after await -
  const scope = effectScope()
  const initialTosHash =
    cachedTosHash.value?.versionHash ??
    (await fetchAgreementHash(() => queryClient.fetchQuery(latestTermsOfServiceQueryOptions)))
  const initialPrivacyPolicyHash =
    cachedPrivacyPolicyHash.value?.versionHash ??
    (await fetchAgreementHash(() => queryClient.fetchQuery(latestPrivacyPolicyQueryOptions)))
  if (initialTosHash == null || initialPrivacyPolicyHash == null) return undefined

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

    const userAgreed = () => {
      localStorage.set('termsOfService', { versionHash: tosHash.value })
      localStorage.set('privacyPolicy', { versionHash: privacyPolicyHash.value })
    }

    return proxyRefs({
      agreedToTos,
      agreedToPrivacyPolicy,
      userAgreed,
    })
  })!
}
