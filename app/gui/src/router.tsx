import { CloudBrowserDisabledLayout } from '#/layouts/CloudBrowserDisabled'
import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import Login from '#/pages/authentication/Login'
import ResetPassword from '#/pages/authentication/ResetPassword'
import RestoreAccount from '#/pages/authentication/RestoreAccount'
import { Setup } from '#/pages/authentication/Setup'
import { Subscribe } from '#/pages/subscribe/Subscribe'
import { SubscribeSuccess } from '#/pages/subscribe/SubscribeSuccess'
import {
  CONFIRM_REGISTRATION_PATH,
  DASHBOARD_PATH,
  FORGOT_PASSWORD_PATH,
  LOGIN_PATH,
  RESET_PASSWORD_PATH,
  RESTORE_USER_PATH,
  SETUP_PATH,
  SUBSCRIBE_PATH,
  SUBSCRIBE_SUCCESS_PATH,
} from '$/appUtils'
import { UserSessionType } from '$/providers/auth'
import { flagsStore } from '$/providers/featureFlags'
import { reactComponent } from '@/util/react'
import * as vueQuery from '@tanstack/vue-query'
import { defineComponent, DefineComponent, effectScope, h } from 'vue'
import { createRouter, createWebHistory } from 'vue-router'

declare module 'vue' {
  interface ComponentCustomOptions {
    dataLoader?: (this: undefined, queryClient: vueQuery.QueryClient) => Promise<any>
  }
}

function withDataLoader(
  componentPromise: () => Promise<{
    default: DefineComponent<any>
  }>,
) {
  return async () => {
    const { default: component } = await componentPromise()
    const scope = effectScope()
    let data: any

    return defineComponent({
      async beforeRouteEnter(to, from) {
        const queryClient = vueQuery.useQueryClient()
        console.debug('Data loading')
        const result = await component.beforeRouteEnter?.bind(undefined)(to, from, () =>
          console.error('Do not use next in withDataLoader'),
        )
        console.debug('Result', result)
        if (result !== true && result !== undefined) return result
        data = await scope.run(() => component.dataLoader?.bind(undefined)(queryClient))
        console.debug('Set data to', data)
        return result
      },
      unmounted() {
        scope.stop()
      },
      render() {
        console.debug('Rendering', data)
        return h(component, data)
      },
    })
  }
}

const UNAVAILABLE_PATH = '/UNAVAILABLE'

function requireCloudBrowserEnabled() {
  const isCloudExecutionEnabled = flagsStore.getState().featureFlags.enableCloudExecution
  if (!isCloudExecutionEnabled) {
    return { name: 'cloudDisabled' }
  }
}

// TODO[ao]: Now the React Layouts are wrapped and used here, but they should be gradually replaced
// with vue-router guards
// (https://router.vuejs.org/guide/advanced/navigation-guards.html#Per-Route-Guard or similar).
const routes = [
  {
    path: UNAVAILABLE_PATH,
    component: withDataLoader(() => import('$/components/ProtectedLayout.vue') as any),
    children: [
      { path: LOGIN_PATH, component: reactComponent(Login), meta: { access: 'guest' as const } },
      {
        path: '/registration',
        component: withDataLoader(() => import('$/components/RegistrationPage.vue') as any),
        meta: { access: 'guest' as const },
      },
      {
        path: UNAVAILABLE_PATH,
        meta: { access: UserSessionType.full },
        component: withDataLoader(() => import('$/components/AppContainerLayout.vue') as any),
        beforeEnter: requireCloudBrowserEnabled,
        children: [
          {
            path: DASHBOARD_PATH,
            component: () =>
              import('#/pages/dashboard/Dashboard').then((comp) => reactComponent(comp.default)),
          },
          {
            path: SUBSCRIBE_PATH,
            component: reactComponent(Subscribe),
          },
        ],
      },
      {
        path: SUBSCRIBE_SUCCESS_PATH,
        meta: { access: UserSessionType.full },
        component: reactComponent(SubscribeSuccess),
      },
      {
        path: RESTORE_USER_PATH,
        meta: { access: 'deleted' as const },
        component: reactComponent(RestoreAccount),
      },
      {
        path: SETUP_PATH,
        meta: { access: 'anyLoggedIn' as const },
        beforeEnter: requireCloudBrowserEnabled,
        component: reactComponent(Setup),
      },
    ],
  },

  /* Other pages are visible to unauthenticated and authenticated users. */
  {
    path: CONFIRM_REGISTRATION_PATH,
    component: reactComponent(ConfirmRegistration),
  },
  {
    path: FORGOT_PASSWORD_PATH,
    component: reactComponent(ForgotPassword),
  },
  {
    path: RESET_PASSWORD_PATH,
    component: reactComponent(ResetPassword),
  },
  {
    path: '/:anyPath(.*)*',
    redirect: '/',
  },
  {
    path: '/',
    name: 'cloudDisabled',
    component: reactComponent(CloudBrowserDisabledLayout),
    props: { redirectPath: DASHBOARD_PATH },
  },
]

const router = createRouter({
  history: createWebHistory(),
  routes,
})

router.onError((error) => console.error('Router error', error))

export default router
