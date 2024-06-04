/**
 * @file
 *
 * This file contains the main router for the application.
 */
import * as React from 'react'

import * as reactRouterDom from 'react-router-dom'

import * as appUtils from '#/appUtils'

import * as authProvider from '#/providers/AuthProvider'

import ConfirmRegistration from '#/pages/authentication/ConfirmRegistration'
import ForgotPassword from '#/pages/authentication/ForgotPassword'
import Login from '#/pages/authentication/Login'
import Registration from '#/pages/authentication/Registration'
import ResetPassword from '#/pages/authentication/ResetPassword'
import RestoreAccount from '#/pages/authentication/RestoreAccount'
import SetUsername from '#/pages/authentication/SetUsername'
import dashboard from '#/pages/dashboard/Dashboard'
import * as subscribe from '#/pages/subscribe/Subscribe'
import * as subscribeSuccess from '#/pages/subscribe/SubscribeSuccess'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'

import * as setOrganizationNameModal from '#/modals/SetOrganizationNameModal'
import * as termsOfServiceModal from '#/modals/TermsOfServiceModal'

import type * as app from './App'

/**
 * Props for the main router for the application.
 */
export interface RoutesProps extends app.AppRouterProps {
  readonly shouldShowDashboard: boolean
  readonly fallback?: React.ReactNode
  readonly basename: string
}

/**
 * The main router for the application.
 */
export function Routes(props: RoutesProps) {
  const { shouldShowDashboard, fallback, basename } = props

  const routes = (
    <>
      {/* Login & registration pages are visible to unauthenticated users. */}
      <reactRouterDom.Route element={<authProvider.GuestLayout />}>
        <reactRouterDom.Route path={appUtils.REGISTRATION_PATH} element={<Registration />} />
        <reactRouterDom.Route path={appUtils.LOGIN_PATH} element={<Login />} />
      </reactRouterDom.Route>

      {/* Protected pages are visible to authenticated users. */}
      <reactRouterDom.Route element={<authProvider.NotDeletedUserLayout />}>
        <reactRouterDom.Route element={<authProvider.ProtectedLayout />}>
          <reactRouterDom.Route element={<termsOfServiceModal.TermsOfServiceModal />}>
            <reactRouterDom.Route element={<setOrganizationNameModal.SetOrganizationNameModal />}>
              {shouldShowDashboard && <reactRouterDom.Route {...dashboard} />}

              <reactRouterDom.Route
                path={appUtils.SUBSCRIBE_PATH}
                errorElement={<errorBoundary.ErrorBoundary />}
                element={
                  <React.Suspense fallback={<loader.Loader />}>
                    <subscribe.Subscribe />
                  </React.Suspense>
                }
              />
            </reactRouterDom.Route>
          </reactRouterDom.Route>

          <reactRouterDom.Route
            path={appUtils.SUBSCRIBE_SUCCESS_PATH}
            element={
              <errorBoundary.ErrorBoundary>
                <React.Suspense fallback={<loader.Loader />}>
                  <subscribeSuccess.SubscribeSuccess />
                </React.Suspense>
              </errorBoundary.ErrorBoundary>
            }
          />
        </reactRouterDom.Route>
      </reactRouterDom.Route>

      <reactRouterDom.Route element={<termsOfServiceModal.TermsOfServiceModal />}>
        {/* Semi-protected pages are visible to users currently registering. */}
        <reactRouterDom.Route element={<authProvider.NotDeletedUserLayout />}>
          <reactRouterDom.Route element={<authProvider.SemiProtectedLayout />}>
            <reactRouterDom.Route path={appUtils.SET_USERNAME_PATH} element={<SetUsername />} />
          </reactRouterDom.Route>
        </reactRouterDom.Route>
      </reactRouterDom.Route>

      {/* Other pages are visible to unauthenticated and authenticated users. */}
      <reactRouterDom.Route
        path={appUtils.CONFIRM_REGISTRATION_PATH}
        element={<ConfirmRegistration />}
      />
      <reactRouterDom.Route path={appUtils.FORGOT_PASSWORD_PATH} element={<ForgotPassword />} />
      <reactRouterDom.Route path={appUtils.RESET_PASSWORD_PATH} element={<ResetPassword />} />

      {/* Soft-deleted user pages are visible to users who have been soft-deleted. */}
      <reactRouterDom.Route element={<authProvider.ProtectedLayout />}>
        <reactRouterDom.Route element={<authProvider.SoftDeletedUserLayout />}>
          <reactRouterDom.Route path={appUtils.RESTORE_USER_PATH} element={<RestoreAccount />} />
        </reactRouterDom.Route>
      </reactRouterDom.Route>

      {/* 404 page */}
      <reactRouterDom.Route path="*" element={<reactRouterDom.Navigate to="/" replace />} />
    </>
  )

  const router = reactRouterDom.createBrowserRouter(
    reactRouterDom.createRoutesFromElements(routes),
    { basename }
  )

  return <reactRouterDom.RouterProvider router={router} fallbackElement={fallback} />
}
