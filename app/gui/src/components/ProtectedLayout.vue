<script lang="ts">
/**
 * @file A component watching changes in current user state. It hides subcomponents and redirects
 * if user lost privileges to see them.
 */
import { EnsoDevtools as EnsoDevToolsReact } from '#/components/Devtools'
import {
  AgreementsModal as AgreementsModalReact,
  type AgreementsModalProps,
} from '#/modals/AgreementsModal'
import LocalStorage from '#/utilities/LocalStorage'
import { DASHBOARD_PATH, LOGIN_PATH, RESTORE_USER_PATH, SETUP_PATH } from '$/appUtils'
import { useUserAgrements } from '$/composables/userAgreements'
import { AuthStore, useAuth, UserSessionType } from '$/providers/auth'
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
  ref,
  watch,
  watchEffect,
  watchPostEffect,
} from 'vue'
import { RouteLocation, useRoute, useRouter } from 'vue-router'

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
      return route.meta.access === auth.session?.type && !auth.isUserMarkedForDeletion()
  }
}

function redirect(auth: AuthStore, localStorage: LocalStorage) {
  if (auth.session == null || auth.isUserDeleted()) return { path: LOGIN_PATH }
  if (auth.isUserSoftDeleted()) return { path: RESTORE_USER_PATH }
  if (auth.session.type === UserSessionType.partial) return { path: SETUP_PATH }
  if (auth.session.type === UserSessionType.full)
    return { path: localStorage.consume('loginRedirect') ?? DASHBOARD_PATH }
  return undefined
}

export default {
  async beforeRouteEnter(to, _from, next) {
    const localStorage = LocalStorage.getInstance()
    const queryClient = vueQuery.useQueryClient()
    const auth = useAuth()
    await auth.waitForSession()

    if (!routeAllowed(to, auth)) {
      const redirectVal = redirect(auth, localStorage)
      return redirectVal ? next(redirectVal) : next(false)
    }

    if (auth.session != null) {
      const scope = effectScope()
      const agreementsModalProps = await scope.run(() => useUserAgrements(queryClient))
      return next((component) => {
        component.routeScope = scope
        scope.run(() => watchEffect(() => (component.agreementsModalProps = agreementsModalProps)))
      })
    }
    return next(true)
  },

  beforeRouteLeave() {
    this.routeScope?.stop()
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
const displayDevTools = computed(() => auth.session?.type === UserSessionType.full)
const tosAndPp = ref<{ composable: TosAndPP; scope: EffectScope }>()

onUnmounted(() => {
  tosAndPp.value?.scope.stop()
})

const agreementsModalProps = ref<AgreementsModalProps>()
const shouldDisplayAgreementsModal = computed(
  () =>
    !(agreementsModalProps.value?.agreedToTos && agreementsModalProps.value.agreedToPrivacyPolicy),
)

defineExpose({
  agreementsModalProps,
  routeScope: undefined as EffectScope | undefined,
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
    v-if="allowed && agreementsModalProps && shouldDisplayAgreementsModal"
    v-bind="agreementsModalProps"
  />
  <RouterView v-else-if="allowed" />

  <EnsoDevtools v-if="displayDevTools" />
</template>
