<script lang="ts">
/**
 * @file A component watching changes in current user state. It hides subcomponents and redirects
 * if user lost privileges to see them.
 */
import { EnsoDevtools as EnsoDevToolsReact } from '#/components/Devtools'
import { AgreementsModal as AgreementsModalReact } from '#/modals/AgreementsModal'
import LocalStorage from '#/utilities/LocalStorage'
import { useAuth, UserSessionType } from '$/providers/auth'
import { useSession } from '$/providers/session'
import { useText } from '$/providers/text'
import { Dialog, reactComponent, ResultComponent } from '@/util/react'
import * as vueQuery from '@tanstack/vue-query'
import { useQueryClient } from '@tanstack/vue-query'
import {
  computed,
  effectScope,
  EffectScope,
  onUnmounted,
  proxyRefs,
  ref,
  watch,
  watchPostEffect,
} from 'vue'
import { useRoute, useRouter } from 'vue-router'
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

export const latestTermsOfServiceQueryOptions = vueQuery.queryOptions({
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

export const latestPrivacyPolicyQueryOptions = vueQuery.queryOptions({
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

type TosAndPP = Awaited<ReturnType<typeof useTosAndPp>>
const useTosAndPp = async (queryClient: vueQuery.QueryClient) => {
  const localStorage = LocalStorage.getInstance()
  const cachedTosHash = computed(() => localStorage.get('termsOfService'))
  const cachedPrivacyPolicyHash = computed(() => localStorage.get('privacyPolicy'))

  const tosQueryOptions = {
    ...latestTermsOfServiceQueryOptions,
    ...(cachedTosHash.value?.versionHash != null && {
      initialData: { hash: cachedTosHash.value.versionHash },
    }),
  }

  const privacyPolicyQueryOptions = {
    ...latestPrivacyPolicyQueryOptions,
    ...(cachedPrivacyPolicyHash.value?.versionHash != null && {
      initialData: { hash: cachedPrivacyPolicyHash.value.versionHash },
    }),
  }

  await Promise.all([
    queryClient.ensureQueryData(tosQueryOptions),
    queryClient.ensureQueryData(privacyPolicyQueryOptions),
  ])

  const { data: tosHash } = vueQuery.useQuery(tosQueryOptions, queryClient)
  const { data: privacyPolicyHash } = vueQuery.useQuery(privacyPolicyQueryOptions, queryClient)

  const agreedToTos = computed(() => tosHash.value === cachedTosHash.value?.versionHash)
  const agreedToPrivacyPolicy = computed(
    () => privacyPolicyHash.value === cachedPrivacyPolicyHash.value?.versionHash,
  )

  const shouldDisplay = computed(() => !(agreedToTos.value && agreedToPrivacyPolicy.value))

  function onSubmit() {
    if (tosHash.value != null && privacyPolicyHash.value != null) {
      localStorage.set('termsOfService', { versionHash: tosHash.value })
      localStorage.set('privacyPolicy', { versionHash: privacyPolicyHash.value })
    }
  }
  return proxyRefs({
    agreedToTos,
    agreedToPrivacyPolicy,
    shouldDisplay,
    onSubmit,
  })
}

const AgreementsModal = reactComponent(AgreementsModalReact)

let scope: EffectScope

export default {
  async beforeRouteEnter(to, _from, next) {
    const queryClient = vueQuery.useQueryClient()
    const auth = useAuth()
    await auth.waitForSession()
    const guard = auth.routeGuard(to)
    const router = useRouter()
    if (!guard.allowed && guard.redirect) return next(guard.redirect)
    if (!guard.allowed) return next(false)
    if (auth.session != null) {
      scope = effectScope()
      scope.run(() => {
        const tosAndPp = await useTosAndPp(queryClient)
        watchEffect(() => {
          if (tosAndPp.shouldDisplay) {
            router.replace({ name: 'agreementsModal', params: tosAndPp })
          }
        })
      )
    }
    next()

    return next(true)
  },
}
</script>

<script setup lang="ts">
const session = useSession()
const auth = useAuth()
const route = useRoute()
const router = useRouter()
const queryClient = useQueryClient()
const text = useText()
const EnsoDevtools = reactComponent(EnsoDevToolsReact)

const routeGuardResult = computed(() => auth.routeGuard(route))
watch(
  routeGuardResult,
  (result) => {
    if (!result.allowed && result.redirect) {
      router.push(result.redirect)
    }
  },
  { immediate: true },
)

// Once user is logged out, we clear queries. We do it in post effect to make sure all unused
// queries are inactive.
watchPostEffect(() => {
  if (auth.session == null) {
    queryClient.removeQueries({ type: 'inactive' })
    queryClient.nukePersister()
  }
})

const modalProps = computed(() => ({ isOpen: session.isLoggingOut }))
const displayDevTools = computed(() => auth.session?.type === UserSessionType.full)
const tosAndPp = ref<{ composable: TosAndPP; scope: EffectScope }>()

onUnmounted(() => {
  tosAndPp.value?.scope.stop()
})

defineExpose({
  setTosAndPp(composable: TosAndPP, scope: EffectScope) {
    tosAndPp.value?.scope.stop()
    tosAndPp.value = { composable, scope }
  },
})
</script>

<template>
  <div v-if="auth.session == null" data-testid="before-auth-layout" aria-hidden>
    <!-- This div is used as a flag to indicate that the user is not logged in.
        also it guarantees that the top-level suspense boundary is already resolved -->
  </div>
  <div
    v-if="auth.session?.type === UserSessionType.full"
    data-testid="after-auth-layout"
    aria-hidden
  >
    <!--This div is used as a flag to indicate that the dashboard has been loaded and the user is authenticated. */}
        also it guarantees that the top-level suspense boundary is already resolved -->
  </div>

  <Dialog
    :aria-label="text.getText('loggingOut')"
    :isDismissable="false"
    :isKeyboardDismissDisabled="true"
    :hideCloseButton="true"
    :modalProps="modalProps"
  >
    <ResultComponent status="loading" :title="text.getText('loggingOut')" />
  </Dialog>

  <AgreementsModal
    v-if="routeGuardResult.allowed && tosAndPp?.composable.shouldDisplay"
    v-bind="tosAndPp.composable"
  />
  <RouterView v-else-if="routeGuardResult.allowed" />

  <EnsoDevtools v-if="displayDevTools" />
</template>
