import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { OpenAppWatcher } from '#/layouts/OpenAppWatcher'
import {
  AgreementsModal,
  latestPrivacyPolicyQueryOptions,
  latestTermsOfServiceQueryOptions,
} from '#/modals/AgreementsModal'
import { InvitedToOrganizationModal } from '#/modals/InvitedToOrganizationModal'
import { SetupOrganizationAfterSubscribe } from '#/modals/SetupOrganizationAfterSubscribe'
import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import Login from '#/pages/authentication/Login'
import Registration from '#/pages/authentication/Registration'
import ResetPassword from '#/pages/authentication/ResetPassword'
import RestoreAccount from '#/pages/authentication/RestoreAccount'
import { Setup } from '#/pages/authentication/Setup'
import Dashboard from '#/pages/dashboard/Dashboard'
import { Subscribe } from '#/pages/subscribe/Subscribe'
import { SubscribeSuccess } from '#/pages/subscribe/SubscribeSuccess'
import { CloudBrowserDisabledLayout as CloudBrowserDisabledLayoutImpl } from '#/providers/AuthProvider'
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
import { reactComponent } from '@/util/react'
import * as vueQuery from '@tanstack/vue-query'
import { PropsWithChildren, ReactNode } from 'react'
import { createRouter, createWebHistory, RouteRecordRaw } from 'vue-router'
import ReactLayoutWrapper from './components/ReactLayoutWrapper.vue'
import { useAuth, UserSessionType } from './providers/auth'
import { useLocalStorage } from './stores/localStorage'

/**
 * Wrap react component in ErrorBoundary and Suspense.
 *
 * The Router views doesn't really like things thrown at them from react.
 */
function wrapReactForRouter(Component: (props: PropsWithChildren) => ReactNode) {
  return ({ children }: PropsWithChildren) => (
    <ErrorBoundary>
      <Suspense>
        <Component>{children}</Component>
      </Suspense>
    </ErrorBoundary>
  )
}

function reactForRouter(component: () => ReactNode) {
  return reactComponent(wrapReactForRouter(component))
}

/**
 * Nests multiple ReactLayouts and create a Route.
 *
 * This function avoids making too many vue-react boundaries.
 */
function applyLayouts(
  components: ((props: PropsWithChildren) => ReactNode)[],
  children: RouteRecordRaw[],
) {
  const reducedComponent = components.reduceRight((composed, Next) => (props) => (
    <Next>{composed(props)}</Next>
  ))
  return {
    component: ReactLayoutWrapper,
    props: {
      reactComponent: wrapReactForRouter(reducedComponent),
    },
    path: '/UNAVAILABLE',
    children,
  }
}

function CloudBrowserDisabledLayout(props: PropsWithChildren) {
  return (
    <CloudBrowserDisabledLayoutImpl redirectPath={SETUP_PATH}>
      {props.children}
    </CloudBrowserDisabledLayoutImpl>
  )
}

async function notDeletedUser() {
  const auth = useAuth()
  if (await auth.isUserMarkedForDeletion()) {
    return { path: RESTORE_USER_PATH }
  }
}

async function softDeletedUser() {
  const auth = useAuth()
  if (await auth.isUserMarkedForDeletion()) {
    const isSoftDeleted = await auth.isUserSoftDeleted()
    const isDeleted = await auth.isUserDeleted()
    if (isSoftDeleted) {
      return true
    } else if (isDeleted) {
      return { path: LOGIN_PATH }
    } else {
      return { path: DASHBOARD_PATH }
    }
  }
}

async function prefetchAgreements() {
  const queryClient = vueQuery.useQueryClient()
  await queryClient.ensureQueryData(latestTermsOfServiceQueryOptions)
  await queryClient.ensureQueryData(latestPrivacyPolicyQueryOptions)
}

// TODO[ao]: Now the React Layouts are wrapped and used here, but they should be gradually replaced
// with vue-router guards
// (https://router.vuejs.org/guide/advanced/navigation-guards.html#Per-Route-Guard or similar).
const routes = [
  {
    path: '/UNAVAILABLE',
    meta: { access: 'guest' as const },
    children: [
      { path: LOGIN_PATH, component: reactForRouter(Login) },
      { path: '/registration', component: reactForRouter(Registration) },
    ],
  },
  {
    path: '/UNAVAILABLE',
    meta: { access: UserSessionType.full },
    children: [
      {
        path: '/UNAVAILABLE',
        beforeEnter: notDeletedUser,
        children: [
          {
            beforeEnter: prefetchAgreements,
            ...applyLayouts(
              [
                ({ children }) => <AgreementsModal>{children}</AgreementsModal>,
                CloudBrowserDisabledLayout,
                SetupOrganizationAfterSubscribe,
                InvitedToOrganizationModal,
                OpenAppWatcher,
              ],
              [
                {
                  path: DASHBOARD_PATH,
                  component: reactForRouter(Dashboard),
                },
                {
                  path: SUBSCRIBE_PATH,
                  component: reactForRouter(Subscribe),
                },
              ],
            ),
          },
          {
            path: SUBSCRIBE_SUCCESS_PATH,
            component: reactForRouter(SubscribeSuccess),
          },
        ],
      },
      {
        path: '/UNAVAILABLE',
        beforeEnter: softDeletedUser,
        children: [
          {
            path: RESTORE_USER_PATH,
            component: reactForRouter(RestoreAccount),
          },
        ],
      },
    ],
  },
  {
    path: '/UNAVAILABLE',
    meta: { access: 'anyLoggedIn' as const },
    beforeEnter: [prefetchAgreements, notDeletedUser],
    children: [
      applyLayouts(
        [
          ({ children }) => <AgreementsModal>{children}</AgreementsModal>,
          CloudBrowserDisabledLayout,
        ],
        [
          {
            path: SETUP_PATH,
            component: reactForRouter(Setup),
          },
        ],
      ),
    ],
  },

  /* Other pages are visible to unauthenticated and authenticated users. */
  {
    path: CONFIRM_REGISTRATION_PATH,
    component: reactForRouter(ConfirmRegistration),
  },
  {
    path: FORGOT_PASSWORD_PATH,
    component: reactForRouter(ForgotPassword),
  },
  {
    path: RESET_PASSWORD_PATH,
    component: reactForRouter(ResetPassword),
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

router.beforeEach(async (to) => {
  const auth = useAuth()
  const localStorage = useLocalStorage()
  await auth.suspense()
  const session = auth.session
  console.log('Routing to ', to.path, ' session ', session)
  if (to.meta.access == null) return true
  if (to.meta.access === 'guest' && session == null) return true
  if (to.meta.access === 'anyLoggedIn' && session != null) return true
  if (to.meta.access === session?.type) return true

  if (session == null) return { path: LOGIN_PATH }
  if (session.type === UserSessionType.partial) return { path: SETUP_PATH }
  if (session.type === UserSessionType.full)
    return { path: localStorage.consume('loginRedirect') ?? DASHBOARD_PATH }
  console.error('ROUTING FAILED', session, to)
  return false
})

export default router
