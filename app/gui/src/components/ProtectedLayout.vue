<script lang="ts">
/**
 * @file A component watching changes in current user state. It hides subcomponents and redirects
 * if user lost privileges to see them. Also makes sure user will agree with Terms of Service and
 * privacy policy.
 */
import {
  EnsoDevtools as EnsoDevToolsReact,
  ReactQueryDevtools as ReactQueryDevtoolsReact,
} from '#/components/Devtools'
import {
  AgreementsModal as AgreementsModalReact,
  type AgreementsModalProps,
} from '#/modals/AgreementsModal'
import LocalStorage from '#/utilities/LocalStorage'
import { DASHBOARD_PATH, LOGIN_PATH, RESTORE_USER_PATH } from '$/appUtils'
import { useUserAgreements } from '$/composables/userAgreements'
import { useAuth, type AuthStore } from '$/providers/auth'
import { useFeatureFlag } from '$/providers/featureFlags'
import { useSession } from '$/providers/session'
import { useText } from '$/providers/text'
import type { DataLoader } from '$/router'
import { useAppClass } from '@/providers/appClass'
import { Dialog, reactComponent, ResultComponent } from '@/util/react'
import * as vueQuery from '@tanstack/vue-query'
import { useQueryClient } from '@tanstack/vue-query'
import { Err, Ok } from 'enso-common/src/utilities/data/result'
import { computed, effectScope, EffectScope, watch, watchPostEffect } from 'vue'
import { useRoute, useRouter, type RouteLocation } from 'vue-router'

declare module 'vue-router' {
  interface RouteMeta {
    access?: 'guest' | 'anyLoggedIn' | 'deleted'
  }
}

const AgreementsModal = reactComponent(AgreementsModalReact)

function routeAllowed(route: RouteLocation, auth: AuthStore) {
  switch (route.meta.access) {
    case null:
      console.error(
        'A route ',
        route,
        'is inside ProtectedLayout but does not specify access level.',
      )
      return true
    case 'guest':
      return auth.session == null
    case 'anyLoggedIn':
      return auth.session != null && !auth.isUserMarkedForDeletion()
    case 'deleted':
      return auth.isUserSoftDeleted()
    default:
      return !auth.isUserMarkedForDeletion()
  }
}

function redirect(auth: AuthStore, localStorage: LocalStorage) {
  if (auth.session == null || auth.isUserDeleted()) return { path: LOGIN_PATH }
  if (auth.isUserSoftDeleted()) return { path: RESTORE_USER_PATH }
  return { path: localStorage.consume('loginRedirect') ?? DASHBOARD_PATH }
}

function requireUserAgreements(route: RouteLocation) {
  switch (route.meta.access) {
    case 'deleted':
    case 'guest':
    case undefined:
      return false
    default:
      return true
  }
}

let scope: EffectScope | undefined

type Props = {
  agreementsModalProps: AgreementsModalProps | undefined
}

export const dataLoader: DataLoader<Props> = {
  async beforeRouteEnter(to) {
    const queryClient = vueQuery.useQueryClient()
    const localStorage = LocalStorage.getInstance()
    const auth = useAuth()
    await auth.waitForSession()

    if (!routeAllowed(to, auth)) {
      return Err(redirect(auth, localStorage) ?? false)
    }

    if (requireUserAgreements(to)) {
      scope = effectScope()
      return Ok({ agreementsModalProps: await scope.run(() => useUserAgreements(queryClient)) })
    }
    return Ok({ agreementsModalProps: undefined })
  },

  async beforeRouteUpdate(to, from, data) {
    if (to.meta.access !== from.meta.access) {
      const queryClient = vueQuery.useQueryClient()
      const localStorage = LocalStorage.getInstance()
      const auth = useAuth()
      await auth.waitForSession()
      if (!routeAllowed(to, auth)) {
        return redirect(auth, localStorage) ?? false
      }
      const agreementsRequired = requireUserAgreements(to)
      if (agreementsRequired && data.agreementsModalProps == null) {
        scope?.stop()
        scope = effectScope()
        data.agreementsModalProps = await scope.run(() => useUserAgreements(queryClient))
      } else if (!agreementsRequired && data.agreementsModalProps != null) {
        scope?.stop()
        data.agreementsModalProps = undefined
      }
    }
  },
}
</script>

<script setup lang="ts">
const props = defineProps<Props>()

const session = useSession()
const auth = useAuth()
const route = useRoute()
const router = useRouter()
const queryClient = useQueryClient()
const text = useText()
const EnsoDevtools = reactComponent(EnsoDevToolsReact)
const ReactQueryDevtools = reactComponent(ReactQueryDevtoolsReact)

// Needed by devtools - act on feature flag changes
const debugHoverAreas = useFeatureFlag('debugHoverAreas')
useAppClass(() => ({ debugHoverAreas: debugHoverAreas.value }))

const allowed = computed(() => routeAllowed(route, auth))
watch(
  allowed,
  (allowed) => {
    if (!allowed) {
      const redirectValue = redirect(auth, LocalStorage.getInstance())
      if (redirectValue) router.push(redirectValue)
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
const displayDevTools = computed(() => auth.session != null)

const shouldDisplayAgreementsModal = computed(
  () =>
    !(props.agreementsModalProps?.agreedToTos && props.agreementsModalProps?.agreedToPrivacyPolicy),
)
</script>

<template>
  <div v-if="auth.session == null" data-testid="before-auth-layout" aria-hidden>
    <!-- This div is used as a flag to indicate that the user is not logged in. -->
  </div>
  <div v-else data-testid="after-auth-layout" aria-hidden>
    <!--This div is used as a flag to indicate that the dashboard has been loaded and the user is
    authenticated. -->
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
    v-if="allowed && agreementsModalProps && shouldDisplayAgreementsModal"
    v-bind="agreementsModalProps"
  />
  <RouterView v-else-if="allowed || route.meta.access == null || route.meta.access === 'guest'" />
  <div v-else data-testid="content-not-allowed"></div>

  <EnsoDevtools v-if="displayDevTools" />
  <ReactQueryDevtools v-if="displayDevTools" />
</template>
