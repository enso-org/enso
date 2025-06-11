<script setup lang="ts">
/**
 * @file A component watching changes in current user state. It hides subcomponents and redirects
 * if user lost privileges to see them.
 */

import { EnsoDevtools as EnsoDevToolsReact } from '#/components/Devtools'
import { useAuth, UserSessionType } from '$/providers/auth'
import { useSession } from '$/providers/session'
import { useText } from '$/providers/text'
import { Dialog, reactComponent, ResultComponent } from '@/util/react'
import { useQueryClient } from '@tanstack/vue-query'
import { computed, watch, watchPostEffect } from 'vue'
import { useRoute, useRouter } from 'vue-router'

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

  <RouterView v-if="routeGuardResult.allowed" />

  <EnsoDevtools v-if="displayDevTools" />
</template>
