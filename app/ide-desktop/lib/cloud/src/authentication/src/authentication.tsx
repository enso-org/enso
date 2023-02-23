/**
 * @file Module for authenticating users with AWS Cognito.
 * 
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc.
 */
import { ComponentType, createContext, FC, ReactElement, ReactNode, useCallback, useContext, useEffect, useState } from 'react';
import { Navigate, Outlet, useNavigate, useOutletContext } from 'react-router-dom';
import { toast } from "react-hot-toast"

import { getUsersMe, Organization, SetUsernameBody } from './api';
import * as api from './api';
import { isAmplifyError, isAuthError, Api } from './authentication/api';
import { DASHBOARD_PATH, Logger, LOGIN_PATH, REGISTRATION_PATH, RESET_PASSWORD_PATH, SET_USERNAME_PATH } from './components/app';
import { useAsyncEffect } from './hooks';
import { ListenerCallback } from './authentication/listen';



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

/**
 * URL that the Electron main page is hosted on.
 * 
 * This **must** be the actual page that the Electron app is hosted on, otherwise the OAuth flow
 * will not work and will redirect the user to a blank page. If this is the correct URL, no redirect
 * will occur (which is the desired behaviour).
 */
const MAIN_PAGE_URL = "http://localhost:8080";



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
    signInWithGoogle: () => Promise<void>
    /**
     * Method for signing in a user via their GitHub account.
     *
     * This method will open a new browser window to allow the user to authenticate with GitHub.
     * Once the user has authenticated, the browser window will close, and the user will be signed
     * in.
     */
    signInWithGitHub: () => Promise<void>;
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
    logger: Logger,
    auth: Api,
    /** Callback to execute once the user has authenticated successfully. */
    onAuthenticated: () => void;
    children: ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const AuthProvider = (props: AuthProviderProps): JSX.Element => {
  const { logger, auth, children } = props
  const onAuthenticated = useCallback(props.onAuthenticated, [])
  const navigate = useNavigate();
  const [initialized, setInitialized] = useState(false);
  const [session, setSession] = useState<UserSession | undefined>(undefined);
  // State that, when incremented, forces a refresh of the user session. This is useful when a user
  // has just logged in (so their cached credentials are out of date).
  const [refresh, setRefresh] = useState(0);

  useEffect(() => {
    const listener: ListenerCallback = (event) => {
      if (event === "signIn") {
          setRefresh((refresh) => refresh + 1);
      } else if (event === "customOAuthState" || event === "cognitoHostedUI") {
          // AWS Amplify doesn't provide a way to set the redirect URL for the OAuth flow, so we
          // have to hack it by replacing the URL in the browser's history. This is done because
          // otherwise the user will be redirected to a URL like `enso://auth`, which will not work.
          //
          // See: https://github.com/aws-amplify/amplify-js/issues/3391#issuecomment-756473970
          window.history.replaceState({}, "", MAIN_PAGE_URL)
          setRefresh((refresh) => refresh + 1)
      // Typescript doesn't know that this is an exhaustive match, so we need to disable the
      // `no-unnecessary-condition` rule. We can't just turn the final block into an `else` because
      // then we wouldn't get an error if we added a new event type.
      // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
      } else if (event === "signOut") {
          setSession(undefined)
      }
    };
    
    const cancel = auth.registerAuthEventListener(listener);
    // Return the `cancel` function from the `useEffect`, which ensures that the listener is cleaned
    // up between renders. This must be done because the `useEffect` will be called multiple times
    // during the lifetime of the component.
    return cancel
  }, [auth, logger]);

  // Register an async effect that will fetch the user's session whenever the `refresh` state is
  // incremented. This is useful when a user has just logged in (as their cached credentials are out
  // of date, so this will update them).
  const [authSession] = useAsyncEffect(null, logger, async () => {
    return await auth.userSession();
  }, [refresh])

  // Fetch the JWT access token from the session via the AWS Amplify library.
  //
  // When invoked, retrieves the access token (if available) from the storage method chosen when
  // Amplify was configured (e.g. local storage). If the token is not available, returns
  // `undefined`.  If the token has expired, automatically refreshes the token and returns the new
  // token.
  useEffect(() => {
      const fetchSession = async () => {
        if (!authSession) {
          setInitialized(true);
          setSession(undefined);
          return;
        }
        const { accessToken, email } = authSession;

        // Request the user's organization information from the Cloud backend.
        const organization = await getUsersMe(accessToken);
        console.log("EFFECT: fetchSession 5");

        let session: UserSession;

        if (!organization) {
          session = {
            state: "partial",
            email,
            accessToken,
          }
        } else {
          session = {
            state: "full",
            email,
            accessToken,
            organization
          }

          // Execute the callback that should inform the Electron app that the user has logged in.
          // This is done to transition the app from the authentication/dashboard view to the IDE.
          onAuthenticated()
        }

        setSession(session)
        setInitialized(true)
      };

      // `Auth.currentSession()` throws an error if the user is not signed in. If the user isn't
      // signed in, we can't get the token. For our purposes, we don't care why *exactly* we
      // couldn't get the token, so we catch the error and return `undefined` instead.
      fetchSession()
        .catch((error) => {
          if (isUserFacingError(error)) {
            toast.error(error.message);
          } else {
            logger.error(error)
          }
        });
  }, [authSession, refresh])

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

  const signUp = async (username: string, password: string) => {
    try {
      await auth.signUp(username, password)
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "UsernameExistsException") {
          toast.error(error.message)
          return;
        } else if (error.code === "InvalidPasswordException") {
          toast.error(error.message)
          return;
        }
      }

      throw error;
    }

    toast.success(SIGN_UP_SUCCESS)
  };

  const confirmSignUp = async (email: string, code: string) => {
    try {
      await auth.confirmSignUp(email, code);
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "NotAuthorizedException") {
          if (error.message === "User cannot be confirmed. Current status is CONFIRMED") {
            toast.success(CONFIRM_SIGN_UP_SUCCESS)
            navigate(LOGIN_PATH)
            return;
          }
        }
      }

      throw error;
    }

    navigate(LOGIN_PATH)
    toast.success(CONFIRM_SIGN_UP_SUCCESS)
  } 

  const setUsername = async (accessToken: string, username: string, email: string) => {
      const body: SetUsernameBody = { userName: username, userEmail: email };
      await api.setUsername(accessToken, body);
      navigate(DASHBOARD_PATH);
      toast.success(SET_USERNAME_SUCCESS);
  };

  const signInWithPassword = async (
    email: string,
    password: string,
  ) => {
    try {
      logger.log(email)
      await auth.signInWithPassword(email, password);
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "UserNotFoundException") {
          navigate(REGISTRATION_PATH);
          toast.error("User not found. Please register first.")
          return
        } else if (error.code === "UserNotConfirmedException") {
          toast.error("User not confirmed. Please check your email for a confirmation link.");
          return
        } else if (error.code === "NotAuthorizedException") {
          toast.error("Incorrect username or password.");
          return
        }
      }

      throw error
    }

    // Now that we've logged in, we need to refresh the session so that the user's organization
    // information is available.
    //setRefresh(refresh + 1);

    navigate(DASHBOARD_PATH)
    toast.success(SIGN_IN_WITH_PASSWORD_SUCCESS)
  };

  const forgotPassword = async (email: string) => {
    try {
      await auth.forgotPassword(email);
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "UserNotFoundException") {
          toast.error("User not found. Please register first.");
          return
        } else if (error.code === "InvalidParameterException") {
          if (error.message === "Cannot reset password for the user as there is no registered/verified email or phone_number") {
            toast.error("Cannot reset password for user with unverified email. Please verify your email first.")
            return
          }
        }
      }

      throw error
    }

    navigate(RESET_PASSWORD_PATH)
    toast.success(FORGOT_PASSWORD_SUCCESS)
  }

  const resetPassword = async (email: string, code: string, password: string) => {
    try {
      await auth.forgotPasswordSubmit(email, code, password)
    } catch (error) {
      if (isAuthError(error)) {
        toast.error(error.log)
        return
      }

      if (isAmplifyError(error)) {
        toast.error(error.message)
        return
      }

      throw error
    }

    navigate(LOGIN_PATH)
    toast.success(RESET_PASSWORD_SUCCESS)
  };

  const signOut = async () => {
    await auth.signOut();

    toast.success(SIGN_OUT_SUCCESS)
  }

  const value = {
    signUp: withLoadingToast(signUp),
    confirmSignUp: withLoadingToast(confirmSignUp),
    setUsername,
    signInWithGoogle: auth.signInWithGoogle,
    signInWithGitHub: auth.signInWithGithub,
    signInWithPassword: withLoadingToast(signInWithPassword),
    forgotPassword: withLoadingToast(forgotPassword),
    resetPassword: withLoadingToast(resetPassword),
    signOut,
    session,
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



// ===================
// === withoutUser ===
// ===================

/**
 * A React higher-order component (HOC) used to that can be used to wrap a component that requires
 * that the user is not currently logged in. For example, the login page or the registration page.
 * 
 * What this means is that a component wrapped with this HOC will:
 * 1. redirect the user to the "dashboard" page if they are currently logged in;
 * 2. otherwise render the component as normal.
 */
export const withoutUser = <T extends object>(
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Component: ComponentType<T>,
): FC<T> => (props) => {
  const { session } = useAuth();

  // If the user is logged in, redirect them to the dashboard page.
  if (session) {
    return <Navigate to={DASHBOARD_PATH} />;
  }

  // If the user is not logged in, render the component as normal.
  return <Component {...props} />;
}



// =======================
// === withPartialUser ===
// =======================

/**
 * A React higher-order component (HOC) used to that can be used to wrap a component that requires
 * a user that has registered, but not yet set a username, and is currently logged in.
 * 
 * What this means is that a component wrapped with this HOC will:
 * 1. redirect the user to the "login" page if they are not logged in;
 * 2. redirect the user to the "dashboard" page if they are logged in, and have set a username;
 * 3. otherwise render the component as normal, passing the session information to the component.
 */
export const withPartialUser = <T extends object>(
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Component: ComponentType<T>,
): FC<T> => (props) => {
  const { session } = useAuth();

  // If the user is not logged in, redirect them to the login page.
  if (!session) {
    return <Navigate to={LOGIN_PATH} />;
  }

  // If the user has set a username, redirect them to the "dashboard" page.
  if (session.state == "full") {
    return <Navigate to={DASHBOARD_PATH} state={session} />;
  }

  // If the user is logged in, but has not set a username, render the component as normal.
  //
  // The session is passed in to the component, because it is available to us here, and it is
  // convenient to have it available to the component, without the component having to call
  // `useAuth` and check for authentication itself.
  return <Component session={session} {...props} />;
}

// ================
// === withUser ===
// ================

/**
 * A React higher-order component (HOC) used to that can be used to wrap a component that requires
 * a user that has registered, set a username, and is currently logged in.
 * 
 * What this means is that a component wrapped with this HOC will:
 * 1. redirect the user to the "login" page if they are not logged in;
 * 2. redirect the user to the "set username" page if they are logged in, but have not set a username;
 * 3. otherwise render the component as normal, passing the session information to the component.
 */
export const withUser = <T extends object>(
  // eslint-disable-next-line @typescript-eslint/naming-convention
  Component: ComponentType<T>,
): FC<T> => (props) => {
  const { session } = useAuth();

  // If the user is not logged in, redirect them to the login page.
  if (!session) {
    return <Navigate to={LOGIN_PATH} />;
  }

  // If the user has not set a username, redirect them to the "set username" page.
  if (session.state == "partial") {
    return <Navigate to={SET_USERNAME_PATH} state={session} />;
  }

  // If the user is logged in, render the component as normal.
  //
  // The session is passed in to the component, because it is available to us here, and it is
  // convenient to have it available to the component, without the component having to call
  // `useAuth` and check for authentication itself.
  return <Component session={session} {...props} />;
}



// ======================
// === ProtectedRoute ===
// ======================

interface ProtectedRouteProps {
  isAllowed: boolean;
  redirectPath?: string;
  children?: ReactElement
}

//export const ProtectedRoute: FC<ProtectedRouteProps> = ({ isAllowed, redirectPath = LOGIN_PATH, children }): ReactNode => {
// eslint-disable-next-line @typescript-eslint/naming-convention
export const ProtectedRoute: FC<ProtectedRouteProps> = ({ isAllowed, redirectPath = LOGIN_PATH, children }) => {
  if (!isAllowed) {
    return <Navigate to={redirectPath} />;
  }

  return children ? children : <Outlet />;
}


//export const ProtectedRoute: FC<ProtectedRouteProps> = ({ isAllowed, redirectPath = LOGIN_PATH, children }): ReactNode => {
// eslint-disable-next-line @typescript-eslint/naming-convention
export const ProtectedRoute2: FC<ProtectedRouteProps> = ({ isAllowed, redirectPath = LOGIN_PATH, children }) => {
  if (!isAllowed) {
    return <Navigate to={redirectPath} />;
  }

  return children ? children : <Outlet />;
}




interface Config {
  redirectCondition: (props: any) => boolean;
  redirectPath: string;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const withRedirectIfUnauthorized = (config: Config) => <T extends object>(Component: FC<T>): FC<T> => (props) => {
  const { redirectCondition, redirectPath } = config;

  if (redirectCondition(props)) {
    return <Navigate to={redirectPath} />;
  }

  return <Component {...props} />;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const ProtectedLayout = () => {
  const { session } = useAuth();

  if (!session) {
    return <Navigate to={LOGIN_PATH} />;
  }

  return <Outlet context={ session } />;
}

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

export const usePartialUserSession = () => {
  return useOutletContext<PartialUserSession>();
}

export const useFullUserSession = () => {
  return useOutletContext<FullUserSession>();
}
