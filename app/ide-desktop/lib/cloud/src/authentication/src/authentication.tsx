/**
 * @file Module for authenticating users with AWS Cognito.
 * 
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc.
 */
import { createContext, ReactNode, useCallback, useContext, useEffect, useState } from 'react';
import { Navigate, Outlet, useNavigate, useOutletContext } from 'react-router-dom';
import { toast } from "react-hot-toast"

import { createBackend, Organization, SetUsernameBody } from './api';
import { Api } from './authentication/api';
import { DASHBOARD_PATH, LOGIN_PATH, REGISTRATION_PATH, RESET_PASSWORD_PATH, SET_USERNAME_PATH } from './components/app';
import { useLogger } from './logger';
import { useSession } from './authentication/providers/session';
import { UnreachableCaseError } from './error';



// =============
// === Types ===
// =============


// === UserSession ===

export type UserSession =
  | FullUserSession
  | PartialUserSession;

/// The type of the object containing the currently signed-in user's session data.
export interface FullUserSession {
  /// Literal for narrowing the type of the `Session` object.
  state: "full",
  /// The user's JSON Web Token (JWT), used for authenticating and authorizing requests to the API.
  accessToken: string;
  /// The user's email address.
  email: string;
  /// The user's organization information.
  organization: Organization;
}

/// The type of the object containing the currently signed-in user's session data, if the user has
/// not yet set their username.
///
/// If a user has not yet set their username, they do not yet have an organization associated with
/// their account. Otherwise, this type is identical to the `Session` type. This type should ONLY be
/// used by the `SetUsername` component.
export interface PartialUserSession {
  /// Literal for narrowing the type of the `Session` object.
  state: "partial",
  /// The user's JSON Web Token (JWT), used for authenticating and authorizing requests to the API.
  accessToken: string;
  /// The user's email address.
  email: string;
}



// =================
// === Constants ===
// =================

const SIGN_UP_SUCCESS = "We have sent you an email with further instructions!"
const CONFIRM_SIGN_UP_SUCCESS = "Your account has been confirmed! Please log in."
const SET_USERNAME_SUCCESS = "Your username has been set!"
const SIGN_IN_WITH_PASSWORD_SUCCESS = "Successfully logged in!"
const FORGOT_PASSWORD_SUCCESS = "We have sent you an email with further instructions!"
const RESET_PASSWORD_SUCCESS = "Successfully reset password!"
const SIGN_OUT_SUCCESS = "Successfully logged out!"



// ===================
// === AuthContext ===
// ===================

/**
 * Interface returned by the `useAuth` hook.
 *
 * Contains the currently authenticated user's session data, as well as methods for signing in,
 * signing out, etc. All interactions with the authentication API should be done through this
 * interface.
 */
interface AuthContextType {
    /**
     * Method for signing up a new user with an email address and a password.
     *
     * Does not rely on external identity providers (e.g., Google, GitHub).
     */
    signUp: (email: string, password: string) => Promise<void>;
    /** Method for confirming a user's email address after they attempt to sign up. */
    confirmSignUp: (email: string, code: string) => Promise<void>;
    /**
     * Method for assigning a user a username after they have signed up and confirmed their email.
     */
    setUsername: (accessToken: string, username: string, email: string) => Promise<void>;
    /**
     * Method for signing in a user via their Google account.
     *
     * This method will open a new browser window to allow the user to authenticate with Google.
     * Once the user has authenticated, the browser window will close, and the user will be signed
     * in.
     */
    signInWithGoogle: () => Promise<null>
    /**
     * Method for signing in a user via their GitHub account.
     *
     * This method will open a new browser window to allow the user to authenticate with GitHub.
     * Once the user has authenticated, the browser window will close, and the user will be signed
     * in.
     */
    signInWithGitHub: () => Promise<null>;
    /** Method for signing in a user with an email address and a password. */
    signInWithPassword: (email: string, password: string) => Promise<void>;
    /**
     * Method for starting the password recovery process for a user.
     *
     * This method will send an email to the user with a link to reset their password. The user must
     * click this link to complete the process. Clicking the link will redirect them to the second
     * page of the process (i.e., the "reset password" page), where the confirmation code will
     * already be filled in.
     */
    forgotPassword: (email: string) => Promise<void>;
    /**
     * Method for completing the password recovery process for a user.
     *
     * This method will reset the user's password to the given value. The user must have already
     * started the password recovery process by clicking the link in the email they received.
     */
    resetPassword: (email: string, code: string, password: string) => Promise<void>;
    /** Method for signing out the currently authenticated user. */
    signOut: () => Promise<void>;
    /**
     * Session containing the currently authenticated user's authentication information.
     *
     * If the user has not signed in, the session will be `undefined`.
     */
    session: UserSession | undefined;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const AuthContext = createContext<AuthContextType>(
    {} as AuthContextType
);




// ====================
// === AuthProvider ===
// ====================

export interface AuthProviderProps {
    auth: Api,
    /** Callback to execute once the user has authenticated successfully. */
    onAuthenticated: () => void;
    children: ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const AuthProvider = (props: AuthProviderProps): JSX.Element => {
  const { auth, children } = props
  const { cognito } = auth
  const { session } = useSession();
  const logger = useLogger();
  const onAuthenticated = useCallback(props.onAuthenticated, [])
  const navigate = useNavigate();
  const [initialized, setInitialized] = useState(false);
  const [userSession, setUserSession] = useState<UserSession | undefined>(undefined);

  // Fetch the JWT access token from the session via the AWS Amplify library.
  //
  // When invoked, retrieves the access token (if available) from the storage method chosen when
  // Amplify was configured (e.g. local storage). If the token is not available, returns
  // `undefined`.  If the token has expired, automatically refreshes the token and returns the new
  // token.
  useEffect(() => {
      const fetchSession = async () => {
        if (session.none) {
          setInitialized(true);
          setUserSession(undefined);
          return;
        }
        const { accessToken, email } = session.val;

        const backend = createBackend(accessToken, logger);

        // Request the user's organization information from the Cloud backend.
        const organization = await backend.getUsersMe();

        let userSession: UserSession;

        if (!organization) {
          userSession = {
            state: "partial",
            email,
            accessToken,
          }
        } else {
          userSession = {
            state: "full",
            email,
            accessToken,
            organization
          }

          // Execute the callback that should inform the Electron app that the user has logged in.
          // This is done to transition the app from the authentication/dashboard view to the IDE.
          onAuthenticated()
        }

        setUserSession(userSession)
        setInitialized(true)
      };

      fetchSession()
        .catch((error) => {
          if (isUserFacingError(error)) {
            toast.error(error.message);
          } else {
            logger.error(error)
          }
        });
  }, [session])

  const withLoadingToast = (action: (...args: any) => Promise<void>) => async (...args: any) => {
    const loadingToast = toast.loading("Please wait...")
    try {
      // FIXME [NP]: make this type-safe
      // eslint-disable-next-line @typescript-eslint/no-unsafe-argument
      await action(...args)
    } finally {
      toast.dismiss(loadingToast)
    }
  }

  const signUp = (username: string, password: string) => cognito
    .signUp(username, password)
    .then(result => {
      if (result.ok) {
        toast.success(SIGN_UP_SUCCESS)
      } else {
        toast.error(result.val.message)
      }
    })

  const confirmSignUp = async (email: string, code: string) => cognito 
    .confirmSignUp(email, code)
    .then(result => {
      if (result.err) {
        switch (result.val.kind) {
          case "UserAlreadyConfirmed":
            break;
          default:
            throw new UnreachableCaseError(result.val.kind)
        }
      }

      toast.success(CONFIRM_SIGN_UP_SUCCESS)
      // FIXME [NP]: we do this here & at the call site, is this redundant?
      navigate(LOGIN_PATH)
    })

  const setUsername = async (accessToken: string, username: string, email: string) => {
      const body: SetUsernameBody = { userName: username, userEmail: email };

      // FIXME [NP]: don't create a new API client here, reuse the one from the context.
      const backend = createBackend(accessToken, logger);

      // FIXME [NP]: do we have to refresh after setting the username? In which case we want a
      //   `refresh` dep on the above effect?
      await backend.setUsername(body);
      // FIXME [NP]: do we need this navigate if we're setting the username?
      navigate(DASHBOARD_PATH);
      toast.success(SET_USERNAME_SUCCESS);
  };

  const signInWithPassword = async (
    email: string,
    password: string,
  ) => cognito
    .signInWithPassword(email, password)
    .then(result => {
      if (result.ok) {
        toast.success(SIGN_IN_WITH_PASSWORD_SUCCESS)
        return;
      }

      if (result.val.kind === "UserNotFound") {
        navigate(REGISTRATION_PATH)
      }

      toast.error(result.val.message)
    })

  const forgotPassword = async (email: string) => cognito
    .forgotPassword(email)
    .then(result => {
      if (result.ok) {
        toast.success(FORGOT_PASSWORD_SUCCESS)
        navigate(RESET_PASSWORD_PATH)
      } else {
        toast.error(result.val.message)
      }
    })

  const resetPassword = async (email: string, code: string, password: string) => cognito
    .forgotPasswordSubmit(email, code, password)
    .then(result => {
      if (result.ok) {
        toast.success(RESET_PASSWORD_SUCCESS)
        navigate(LOGIN_PATH)
      } else {
        toast.error(result.val.message)
      }
    })

  const signOut = () => cognito
    .signOut()
    .then(() => toast.success(SIGN_OUT_SUCCESS))
    .then(() => {});

  const value = {
    signUp: withLoadingToast(signUp),
    confirmSignUp: withLoadingToast(confirmSignUp),
    setUsername,
    signInWithGoogle: cognito.signInWithGoogle,
    signInWithGitHub: cognito.signInWithGitHub,
    signInWithPassword: withLoadingToast(signInWithPassword),
    forgotPassword: withLoadingToast(forgotPassword),
    resetPassword: withLoadingToast(resetPassword),
    signOut,
    // FIXME [NP]: why don't these names match?
    session: userSession,
  };

  return (
    <AuthContext.Provider value={value}>
        {/* Only render the underlying app after we assert for the presence of a current user. */}
        {initialized && children}
    </AuthContext.Provider>
  )
};

/**
 * Type of an error containing a `string`-typed `message` field.
 * 
 * Many types of errors fall into this category. We use this type to check if an error can be safely
 * displayed to the user.
 */
interface UserFacingError {
  /** The user-facing error message. */
  message: string;
}

/**
 * Returns `true` if the value is a {@link UserFacingError}.
 */
const isUserFacingError = (value: unknown): value is UserFacingError => {
  return typeof value === "object" && value !== null && "message" in value;
}



// ===============
// === useAuth ===
// ===============

/**
 * A React hook that provides access to the authentication context.
 *
 * Only the hook is exported, and not the context, because we only want to use the hook directly
 * and never the context component.
 */
export const useAuth = () => useContext(AuthContext);



// =======================
// === ProtectedLayout ===
// =======================

// eslint-disable-next-line @typescript-eslint/naming-convention
export const ProtectedLayout = () => {
  const { session } = useAuth();

  if (!session) {
    return <Navigate to={LOGIN_PATH} />;
  }

  return <Outlet context={ session } />;
}



// ===================
// === GuestLayout ===
// ===================

// eslint-disable-next-line @typescript-eslint/naming-convention
export const GuestLayout = () => {
  const { session } = useAuth();

  if (session?.state == "partial") {
    return <Navigate to={SET_USERNAME_PATH} />;
  }

  if (session?.state == "full") {
    return <Navigate to={DASHBOARD_PATH} />;
  }

  return <Outlet />;
}



// =============================
// === usePartialUserSession ===
// =============================

export const usePartialUserSession = () => {
  return useOutletContext<PartialUserSession>();
}



// ==========================
// === useFullUserSession ===
// ==========================

export const useFullUserSession = () => {
  return useOutletContext<FullUserSession>();
}
