/**
 * @file
 *
 * Types for the session provider.
 */

import type * as React from 'react'

import {
  type CognitoUser,
  type ConfirmSignInReturn,
  type ISessionProvider,
  type MfaType,
  type SetupTOTPReturn,
  type UserSession,
  type UserSessionChallenge,
} from '#/authentication/cognito'
import type * as listen from '#/authentication/listen'

/** State contained in a {@link SessionContext}. */
export interface SessionContextType {
  readonly session: UserSession | null
  readonly signUp: (email: string, password: string, organizationId: string | null) => Promise<void>
  readonly confirmSignUp: (email: string, code: string) => Promise<void>
  readonly signInWithGoogle: () => Promise<boolean>
  readonly signInWithGitHub: () => Promise<boolean>
  readonly signInWithPassword: (
    email: string,
    password: string,
  ) => Promise<{
    readonly challenge: UserSessionChallenge
    readonly user: CognitoUser
  }>
  readonly confirmSignIn: (user: CognitoUser, otp: string) => ConfirmSignInReturn
  readonly forgotPassword: (email: string) => Promise<null>
  readonly changePassword: (oldPassword: string, newPassword: string) => Promise<boolean>
  readonly resetPassword: (email: string, code: string, password: string) => Promise<null>
  readonly signOut: () => Promise<void>
  readonly organizationId: () => Promise<string | null>
  readonly getMFAPreference: () => Promise<MfaType>
  readonly updateMFAPreference: (mfaType: MfaType) => Promise<void>
  readonly verifyTotpToken: (otp: string) => Promise<boolean>
  readonly setupTOTP: () => Promise<SetupTOTPReturn>
}

/** Props for a {@link SessionProvider}. */
export interface SessionProviderProps {
  /**
   * The URL that the content of the app is served at, by Electron.
   *
   * This **must** be the actual page that the content is served at, otherwise the OAuth flow will
   * not work and will redirect the user to a blank page. If this is the correct URL, no redirect
   * will occur (which is the desired behaviour).
   *
   * The URL includes a scheme, hostname, and port (e.g., `http://localhost:8080`). The port is not
   * known ahead of time, since the content may be served on any free port. Thus, the URL is
   * obtained by reading the window location at the time that authentication is instantiated. This
   * is guaranteed to be the correct location, since authentication is instantiated when the content
   * is initially served.
   */
  readonly mainPageUrl: URL
  readonly registerAuthEventListener: listen.ListenFunction | null
  readonly authService: ISessionProvider
  readonly onLogout?: () => Promise<void> | void

  readonly children: React.ReactNode | ((props: SessionContextType) => React.ReactNode)
}
