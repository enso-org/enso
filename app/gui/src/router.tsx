import {
  CONFIRM_REGISTRATION_PATH,
  DASHBOARD_PATH,
  FORGOT_PASSWORD_PATH,
  RESET_PASSWORD_PATH,
  RESTORE_USER_PATH,
  SETUP_PATH,
  SUBSCRIBE_PATH,
  SUBSCRIBE_SUCCESS_PATH,
} from '#/appUtils'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Suspense } from '#/components/Suspense'
import { OpenAppWatcher } from '#/layouts/OpenAppWatcher'
import { AgreementsModal } from '#/modals/AgreementsModal'
import { InvitedToOrganizationModal } from '#/modals/InvitedToOrganizationModal'
import { SetupOrganizationAfterSubscribe } from '#/modals/SetupOrganizationAfterSubscribe'
import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import LoadingScreen from '#/pages/authentication/LoadingScreen'
import Login from '#/pages/authentication/Login'
import Registration from '#/pages/authentication/Registration'
import ResetPassword from '#/pages/authentication/ResetPassword'
import RestoreAccount from '#/pages/authentication/RestoreAccount'
import { Setup } from '#/pages/authentication/Setup'
import Dashboard from '#/pages/dashboard/Dashboard'
import { Subscribe } from '#/pages/subscribe/Subscribe'
import { SubscribeSuccess } from '#/pages/subscribe/SubscribeSuccess'
import {
  AnyLoggedInUserLayout,
  CloudBrowserDisabledLayout as CloudBrowserDisabledLayoutImpl,
  GuestLayout,
  NotDeletedUserLayout,
  ProtectedLayout,
  SoftDeletedUserLayout,
} from '#/providers/AuthProvider'
import { PropsWithChildren, ReactNode } from 'react'
import { applyPureReactInVue } from 'veaury'
import { createRouter, createWebHistory, RouteRecordRaw } from 'vue-router'
import ReactLayoutWrapper from './components/ReactLayoutWrapper.vue'

/**
 * Wrap react component in ErrorBoundary and Suspense.
 *
 * The Router views doesn't really like things thrown at them from react.
 */
function wrapReactForRouter(Component: (props: PropsWithChildren) => ReactNode) {
  return ({ children }: PropsWithChildren) => (
    <ErrorBoundary>
      <Suspense fallback={<LoadingScreen />}>
        <Component>{children}</Component>
      </Suspense>
    </ErrorBoundary>
  )
}

function reactForRouter(component: () => ReactNode) {
  return applyPureReactInVue(wrapReactForRouter(component))
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

// TODO[ao]: Now the React Layouts are wrapped and used here, but they should be gradually replaced
// with vue-router guards
// (https://router.vuejs.org/guide/advanced/navigation-guards.html#Per-Route-Guard or similar).
const routes = [
  applyLayouts(
    [GuestLayout],
    [
      { path: '/login', component: reactForRouter(Login) },
      { path: '/registration', component: reactForRouter(Registration) },
    ],
  ),
  applyLayouts(
    [NotDeletedUserLayout, ProtectedLayout],
    [
      applyLayouts(
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
      {
        path: SUBSCRIBE_SUCCESS_PATH,
        component: reactForRouter(SubscribeSuccess),
      },
    ],
  ),
  applyLayouts(
    [
      ({ children }) => <AgreementsModal>{children}</AgreementsModal>,
      AnyLoggedInUserLayout,
      NotDeletedUserLayout,
      CloudBrowserDisabledLayout,
    ],
    [
      {
        path: SETUP_PATH,
        component: reactForRouter(Setup),
      },
    ],
  ),

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
  applyLayouts(
    [ProtectedLayout, SoftDeletedUserLayout],
    [
      {
        path: RESTORE_USER_PATH,
        component: reactForRouter(RestoreAccount),
      },
    ],
  ),
  {
    path: '/:anyPath(.*)*',
    redirect: '/',
  },
]

export default createRouter({
  history: createWebHistory(),
  routes,
})
