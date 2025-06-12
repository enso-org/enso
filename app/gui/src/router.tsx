import { CloudBrowserDisabledLayout } from '#/layouts/CloudBrowserDisabled'
import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import Login from '#/pages/authentication/Login'
import ResetPassword from '#/pages/authentication/ResetPassword'
import RestoreAccount from '#/pages/authentication/RestoreAccount'
import { Setup } from '#/pages/authentication/Setup'
import Dashboard from '#/pages/dashboard/Dashboard'
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
import Registration from '$/components/Registration.vue'
import { UserSessionType } from '$/providers/auth'
import { flagsStore } from '$/providers/featureFlags'
import { reactComponent } from '@/util/react'
import { defineComponent, DefineComponent, effectScope, h, onScopeDispose } from 'vue'
import { createRouter, createWebHistory } from 'vue-router'
import AppContainerLayout from './components/AppContainerLayout.vue'

declare module 'vue' {
  interface ComponentCustomOptions {
    dataLoader?: () => Promise<any>
  }
}

function withDataLoader(componentPromise: () => Promise<{ default: DefineComponent }>) {
  return async () => {
    const block = (await import('$/components/Registration.vue')).default

    const { default: component } = await componentPromise()
    const scope = effectScope()
    const data = await scope.run(() => component.dataLoader?.())

    return defineComponent(() => {
      onScopeDispose(() => scope.stop())

      return () => h(component, data)
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
    component: withDataLoader(() => import('$/components/ProtectedLayout.vue')),
    children: [
      { path: LOGIN_PATH, component: reactComponent(Login), meta: { access: 'guest' as const } },
      {
        path: '/registration',
        component: Registration,
        meta: { access: 'guest' as const },
      },
      {
        path: UNAVAILABLE_PATH,
        meta: { access: UserSessionType.full },
        component: AppContainerLayout,
        beforeEnter: requireCloudBrowserEnabled,
        children: [
          {
            path: DASHBOARD_PATH,
            component: reactComponent(Dashboard),
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
