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
import { createRouter, createWebHistory, RouteRecordRaw, useRoute, useRouter } from 'vue-router'
import ReactLayoutWrapper from './components/ReactLayoutWrapper.vue'
import { createContextForReact } from './providers/react'

function CloudBrowserDisabledLayout(props: PropsWithChildren) {
  return (
    <CloudBrowserDisabledLayoutImpl redirectPath={SETUP_PATH}>
      {props.children}
    </CloudBrowserDisabledLayoutImpl>
  )
}

function BoundedSubscribe() {
  return (
    <ErrorBoundary>
      <Suspense>
        <Subscribe />
      </Suspense>
    </ErrorBoundary>
  )
}

function BoundedSubscribeSuccess() {
  return (
    <ErrorBoundary>
      <Suspense>
        <SubscribeSuccess />
      </Suspense>
    </ErrorBoundary>
  )
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
  const reducedComponent = components.reduceRight(
    (composed, next) => (props: PropsWithChildren) => next({ children: composed(props) }),
  )
  return {
    component: ReactLayoutWrapper,
    props: { reactComponent: reducedComponent },
    path: '/UNAVAILABLE',
    children,
  }
}

const routes = [
  applyLayouts(
    [GuestLayout],
    [
      { path: '/login', component: applyPureReactInVue(Login) },
      { path: '/registration', component: applyPureReactInVue(Registration) },
    ],
  ),
  applyLayouts(
    [NotDeletedUserLayout, ProtectedLayout],
    [
      applyLayouts(
        [
          AgreementsModal,
          CloudBrowserDisabledLayout,
          SetupOrganizationAfterSubscribe,
          InvitedToOrganizationModal,
          OpenAppWatcher,
        ],
        [
          {
            path: DASHBOARD_PATH,
            component: applyPureReactInVue(Dashboard),
          },
          {
            path: SUBSCRIBE_PATH,
            component: applyPureReactInVue(BoundedSubscribe), // TODO: Error boundary and suspense
          },
        ],
      ),
      {
        path: SUBSCRIBE_SUCCESS_PATH,
        component: applyPureReactInVue(BoundedSubscribeSuccess), // TODO: Error boundary and suspense
      },
    ],
  ),
  applyLayouts(
    [AgreementsModal, AnyLoggedInUserLayout, NotDeletedUserLayout, CloudBrowserDisabledLayout],
    [
      {
        path: SETUP_PATH,
        component: applyPureReactInVue(Setup),
      },
    ],
  ),

  /* Other pages are visible to unauthenticated and authenticated users. */
  {
    path: CONFIRM_REGISTRATION_PATH,
    component: applyPureReactInVue(ConfirmRegistration),
  },
  {
    path: FORGOT_PASSWORD_PATH,
    component: applyPureReactInVue(ForgotPassword),
  },
  {
    path: RESET_PASSWORD_PATH,
    component: applyPureReactInVue(ResetPassword),
  },
  applyLayouts(
    [ProtectedLayout, SoftDeletedUserLayout],
    [
      {
        path: RESTORE_USER_PATH,
        component: RestoreAccount,
      },
    ],
  ),
  {
    path: '/.*',
    redirect: '/',
  },
]

export const [useRouterInReact, RouterProviderForReact] = createContextForReact(() => {
  const route = useRoute()
  const queryFlatList = Object.entries(route.query).flatMap(([key, value]) => {
    if (value instanceof Array) {
      return value.map((singleVal) => [key, singleVal ?? ''])
    } else {
      return [[key, value ?? '']]
    }
  })
  return {
    router: useRouter(),
    route,
    searchParams: new URLSearchParams(queryFlatList),
  }
})

export default createRouter({
  history: createWebHistory(),
  routes,
})
