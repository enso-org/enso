import Login from '#/pages/authentication/Login'
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
import { withDataLoader } from '$/router/dataLoader'
import { reactComponent } from '@/util/react'
import { createRouter, createWebHistory } from 'vue-router'

const UNAVAILABLE_PATH = '/UNAVAILABLE'

function requireCloudBrowserEnabled() {
  const isCloudExecutionEnabled = flagsStore.getState().featureFlags.enableCloudExecution
  if (!isCloudExecutionEnabled) {
    return { name: 'cloudDisabled' }
  }
}

const routes = [
  {
    path: UNAVAILABLE_PATH,
    component: withDataLoader(() => import('$/components/ProtectedLayout.vue')),
    children: [
      { path: LOGIN_PATH, component: reactComponent(Login), meta: { access: 'guest' as const } },
      {
        path: '/registration',
        component: withDataLoader(() => import('$/components/RegistrationPage.vue')),
        meta: { access: 'guest' as const },
      },
      {
        path: UNAVAILABLE_PATH,
        meta: { access: UserSessionType.full },
        component: withDataLoader(() => import('$/components/AppContainerLayout.vue')),
        beforeEnter: requireCloudBrowserEnabled,
        children: [
          {
            path: DASHBOARD_PATH,
            component: () =>
              import('#/pages/dashboard/Dashboard').then((mod) => reactComponent(mod.default)),
          },
          {
            path: SUBSCRIBE_PATH,
            component: () =>
              import('#/pages/subscribe/Subscribe').then((mod) => reactComponent(mod.Subscribe)),
          },
        ],
      },
      {
        path: SUBSCRIBE_SUCCESS_PATH,
        meta: { access: UserSessionType.full },
        component: () =>
          import('#/pages/subscribe/SubscribeSuccess').then((mod) =>
            reactComponent(mod.SubscribeSuccess),
          ),
      },
      {
        path: RESTORE_USER_PATH,
        meta: { access: 'deleted' as const },
        component: () =>
          import('#/pages/authentication/RestoreAccount').then((mod) =>
            reactComponent(mod.default),
          ),
      },
      {
        path: SETUP_PATH,
        meta: { access: 'anyLoggedIn' as const },
        beforeEnter: requireCloudBrowserEnabled,
        component: () =>
          import('#/pages/authentication/Setup').then((mod) => reactComponent(mod.Setup)),
      },
      {
        path: '/',
        name: 'cloudDisabled',
        meta: { access: 'anyLoggedIn' as const },
        component: () =>
          import('#/layouts/CloudBrowserDisabled').then((mod) =>
            reactComponent(mod.CloudBrowserDisabledPage),
          ),
        props: { redirectPath: DASHBOARD_PATH },
      },
    ],
  },

  /* Other pages are visible to unauthenticated and authenticated users. */
  {
    path: CONFIRM_REGISTRATION_PATH,
    component: () =>
      import('#/pages/authentication/ConfirmRegistration').then((mod) =>
        reactComponent(mod.default),
      ),
  },
  {
    path: FORGOT_PASSWORD_PATH,
    component: () =>
      import('#/pages/authentication/ForgotPassword').then((mod) => reactComponent(mod.default)),
  },
  {
    path: RESET_PASSWORD_PATH,
    component: () =>
      import('#/pages/authentication/ResetPassword').then((mod) => reactComponent(mod.default)),
  },
  {
    path: '/:anyPath(.*)*',
    redirect: '/',
  },
]

const router = createRouter({
  history: createWebHistory(),
  routes,
})

router.onError((error) => console.error('Router error', error))

export default router
