/** @file Constants for `SessionProvider`. */
import type {
  CognitoUser,
  ConfirmSignInReturn,
  MfaType,
  SetupTOTPReturn,
  UserSession,
  UserSessionChallenge,
} from '#/authentication/cognito'
import { createContext } from 'react'

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

export const SessionContext = createContext<SessionContextType | null>(null)
