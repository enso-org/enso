import Login from '#/pages/authentication/Login'
import {
  CONFIRM_REGISTRATION_PATH,
  DASHBOARD_PATH,
  FORGOT_PASSWORD_PATH,
  LOGIN_PATH,
  PAYMENTS_SUCCESS_PATH,
  REGISTRATION_PATH,
  RESET_PASSWORD_PATH,
  RESTORE_USER_PATH,
  SUBSCRIBE_PATH,
} from '$/appUtils'
import { useAuth } from '$/providers/auth'
import { useConfig } from '$/providers/config'
import { flagsStore } from '$/providers/featureFlags'
import {
  maybeRedirectToProject,
  maybeRedirectToTab,
  openTab,
  redirectFromPath,
} from '$/router/dashboardGuards'
import { withDataLoader } from '$/router/dataLoader'
import { reactComponent, suspendedReactComponent } from '@/util/react'
import { createRouter, createWebHistory, type RouteRecordRaw } from 'vue-router'

const UNAVAILABLE_PATH = '/UNAVAILABLE'

function requireCloudBrowserEnabled() {
  const isCloudExecutionEnabled = flagsStore.getState().featureFlags.enableCloudExecution
  if (!isCloudExecutionEnabled) return { name: 'cloudDisabled' }
}

const routes = [
  {
    path: UNAVAILABLE_PATH,
    component: withDataLoader(() => import('$/components/ProtectedLayout.vue')),
    children: [
      {
        path: LOGIN_PATH,
        meta: { access: 'guest' },
        component: reactComponent(Login),
      },
      {
        path: REGISTRATION_PATH,
        meta: { access: 'guest' },
        component: withDataLoader(() => import('$/components/RegistrationPage.vue')),
      },
      {
        path: UNAVAILABLE_PATH,
        meta: { access: 'anyLoggedIn' },
        component: withDataLoader(() => import('$/components/AppContainerLayout.vue')),
        beforeEnter: requireCloudBrowserEnabled,
        children: [
          {
            name: 'dashboard',
            path: '/',
            beforeEnter: [maybeRedirectToProject, maybeRedirectToTab],
            component: () =>
              import('#/pages/dashboard/Dashboard.tsx').then((mod) =>
                reactComponent(mod.Dashboard),
              ),
            children: [
              {
                name: 'project',
                path: 'project/:id',
                component: () => import('$/project-view/ProjectView.vue'),
              },
              {
                name: 'settings',
                path: 'settings',
                component: () =>
                  import('#/layouts/Settings').then((mod) => suspendedReactComponent(mod.Settings)),
              },
              {
                name: 'ensoPath',
                path: 'asset/:path(.*)*',
                beforeEnter: redirectFromPath,
                component: [],
              },
            ],
          },
          {
            path: SUBSCRIBE_PATH,
            component: () =>
              import('#/pages/subscribe/Subscribe').then((mod) => reactComponent(mod.Subscribe)),
          },
        ],
      },
      {
        path: RESTORE_USER_PATH,
        meta: { access: 'deleted' },
        component: () =>
          import('#/pages/authentication/RestoreAccount').then((mod) =>
            reactComponent(mod.default),
          ),
      },
      {
        path: '/cloudDisabled',
        name: 'cloudDisabled',
        meta: { access: 'anyLoggedIn' },
        component: () =>
          import('#/layouts/CloudBrowserDisabled').then((mod) =>
            reactComponent(mod.CloudBrowserDisabledPage),
          ),
        props: { redirectPath: DASHBOARD_PATH },
      },
    ],
  },
  {
    path: PAYMENTS_SUCCESS_PATH,
    meta: { access: 'anyLoggedIn' },
    component: () =>
      import('#/pages/PaymentsSuccess').then((mod) => reactComponent(mod.PaymentsSuccess)),
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
] satisfies readonly RouteRecordRaw[]

const router = createRouter({
  history: createWebHistory(),
  routes,
})

router.beforeEach(async () => {
  const config = useConfig()
  await config.waitForRemoteConfig()
})
router.beforeEach(async (to, from) => {
  if (to.meta.access !== from.meta.access) {
    await useAuth().waitForSession()
  }
})
router.beforeEach(openTab)

router.onError((error) => console.error('Router error', error))

export default router
