/**
 * @file Module for authenticating users with AWS Cognito.
 * 
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc.
 */
import { Auth, CognitoHostedUIIdentityProvider, SignUpParams } from '@aws-amplify/auth'
import { ComponentType, createContext, FC, ReactElement, ReactNode, useContext, useEffect, useState } from 'react';
import { Navigate, Outlet, useNavigate, useOutletContext } from 'react-router-dom';
import { getUsersMe, Organization, SetUsernameBody } from './api';
import * as api from './api';
import authApi from './authentication/api';
import { DASHBOARD_PATH, LOGIN_PATH, REGISTRATION_PATH, RESET_PASSWORD_PATH, SET_USERNAME_PATH } from './components/app';
import { toast } from "react-hot-toast"
import { AuthError } from '@aws-amplify/auth/lib-esm/Errors';



// =============
// === Types ===
// =============


// === AuthOptions ===

/// The type of the object consumed by the `Auth.configure` method.
type AuthOptions = ReturnType<typeof Auth['configure']>


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


// === AmplifyError ===

/// List of known error codes returned by the AWS Amplify library.
type AmplifyErrorCode =
  | "UserNotFoundException"
  | "UserNotConfirmedException"
  | "NotAuthorizedException"
  | "InvalidPasswordException"
  | "UsernameExistsException"
  | "NetworkError";

/// The type of the object returned by the AWS Amplify library when an error occurs.
interface AmplifyError {
  code: AmplifyErrorCode,
  name: string,
  message: string,
}

/// Function for determining if an error is an `AmplifyError`.
///
/// This function is used to know if we can safely cast an error to an `AmplifyError`.
const isAmplifyError = (error: unknown): error is AmplifyError => {
  if (error && typeof error === "object") {
    return "code" in error && "message" in error && "name" in error;
  }
  return false;
}

/// Function for determining if an error is an `AuthError`.
///
/// This function is used to know if we can safely cast an error to an `AuthError`.
const isAuthError = (error: unknown): error is AuthError => {
  if (error && typeof error === "object") {
    return "name" in error && "log" in error;
  }
  return false;
}



// =================
// === Constants ===
// =================

/// The string used to identify the GitHub federated identity provider in AWS Amplify.
///
/// This provider alone requires a string because it is not a standard provider, and thus has no
/// constant defined in the AWS Amplify library.
const githubProvider = "Github";

/// The configuration for the AWS Amplify library.
///
/// This details user pools, federated identity providers, etc. that are used to authenticate users.
/// The values in this object are not secret, and can be swapped out for testing values to avoid
/// creating authenticated users in the production environment.
// FIXME [NP]: document all these values 
//   See: https://github.com/enso-org/enso/compare/develop...wip/db/cognito-auth-183351503#diff-319a7d209df303b404c07be91bfa179b12aaafcae1c67384dfe3cbffde80e010
// FIXME [NP]: move this to a config file
//const electronAmplifyConfigTesting: AuthOptions = {
//  region: "us-east-1",
//  // FIXME [NP]
//  //identityPoolId: "",
//  userPoolId: "us-east-1_VcFZzGyhv",
//  userPoolWebClientId: "7vic1uoogbq4aq2rve897j0ep0",
//  oauth: {
//    options: {}, // FIXME [NP]
//    domain: "test-enso-pool.auth.us-east-1.amazoncognito.com/",
//    scope: ['email', 'openid'], // FIXME [NP]
//    redirectSignIn: "enso://localhost",
//    redirectSignOut: "enso://localhost",
//    responseType: "code",
//  },
//}
//const browserAmplifyConfigNpekin: AuthOptions = {
//  region: "eu-west-1",
//  // FIXME [NP]
//  //identityPoolId: "",
//  userPoolId: "eu-west-1_sP5bQ4mJs",
//  userPoolWebClientId: "27gd0b05qlnkj1lcsnd0b4fb89",
//  oauth: {
//    options: {}, // FIXME [NP]
//    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
//    domain: "npekin-enso-domain.auth.eu-west-1.amazoncognito.com/",
//    scope: ['email', 'openid'], // FIXME [NP]
//    redirectSignIn: "http://localhost:8081",
//    redirectSignOut: "http://localhost:8081",
//    responseType: "code",
//  },
//}
const browserAmplifyConfigPbuchu: AuthOptions = {
  region: "eu-west-1",
  // FIXME [NP]
  //identityPoolId: "",
  userPoolId: "eu-west-1_jSF1RbgPK",
  userPoolWebClientId: "1bnib0jfon3aqc5g3lkia2infr",
  oauth: {
    options: {}, // FIXME [NP]
    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
    domain: "pb-enso-domain.auth.eu-west-1.amazoncognito.com",
    scope: ['email', 'openid'], // FIXME [NP]
    redirectSignIn: "http://localhost:8081",
    redirectSignOut: "http://localhost:8081",
    responseType: "code",
  },
}
//const browserAmplifyConfigProd: AuthOptions = {
//  region: "eu-west-1",
//  // FIXME [NP]
//  //identityPoolId: "",
//  userPoolId: "eu-west-1_9Kycu2SbD",
//  userPoolWebClientId: "4j9bfs8e7415erf82l129v0qhe",
//  oauth: {
//    options: {}, // FIXME [NP]
//    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
//    domain: "production-enso-domain.auth.eu-west-1.amazoncognito.com/",
//    scope: ['email', 'openid'], // FIXME [NP]
//    redirectSignIn: "https://cloud.enso.org",
//    redirectSignOut: "https://cloud.enso.org",
//    responseType: "code",
//  },
//}
const amplifyConfig = browserAmplifyConfigPbuchu;



// =================
// === Configure ===
// =================

/// Ensure the AWS Amplify library is loaded & configured prior to providing the `AuthContext` to
/// the rest of the app. The AWS Amplify library must be configured before anything else happens
/// because until that is done, we cannot check session status, authenticate users, etc.
///
/// This is top-level code, and will be executed when this module is imported. It must occur at the
/// top level because Amplify uses `require` to load its modules, and `require` cannot be called
/// from within React components.
Auth.configure(amplifyConfig);



// ===================
// === AuthContext ===
// ===================

// FIXME [NP]: document this
interface AuthContextType {
    /// Method for signing up a new user with an email address and a password.
    ///
    /// Does not rely on external identity providers (e.g., Google, GitHub).
    signUp: (email: string, password: string) => Promise<void>;
    /// Method for confirming a user's email address after they attempt to sign up.
    confirmSignUp: (email: string, code: string) => Promise<void>;
    /// Method for assigning a user a username after they have signed up and confirmed their email.
    setUsername: (accessToken: string, username: string, email: string) => Promise<void>;
    /// Method for signing in a user via their Google account.
    ///
    /// This method will open a new browser window to allow the user to authenticate with Google.
    /// Once the user has authenticated, the browser window will close, and the user will be signed
    /// in.
    signInWithGoogle: () => Promise<void>
    /// Method for signing in a user via their GitHub account.
    ///
    /// This method will open a new browser window to allow the user to authenticate with GitHub.
    /// Once the user has authenticated, the browser window will close, and the user will be signed
    /// in.
    signInWithGitHub: () => Promise<void>;
    /// Method for signing in a user with an email address and a password.
    signInWithPassword: (email: string, password: string) => Promise<void>;
    /// Method for starting the password recovery process for a user.
    ///
    /// This method will send an email to the user with a link to reset their password. The user
    /// must click this link to complete the process. Clicking the link will redirect them to the
    /// second page of the process (i.e., the "reset password" page), where the confirmation code
    /// will already be filled in.
    forgotPassword: (email: string) => Promise<void>;
    /// Method for completing the password recovery process for a user.
    ///
    /// This method will reset the user's password to the given value. The user must have already
    /// started the password recovery process by clicking the link in the email they received.
    resetPassword: (email: string, code: string, password: string) => Promise<void>;
    /// Method for signing out the currently authenticated user.
    signOut: () => Promise<void>;
    /// Session containing the currently authenticated user's authentication information.
    ///
    /// If the user has not signed in, the session will be `undefined`.
    session: UserSession | undefined;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const AuthContext = createContext<AuthContextType>(
    {} as AuthContextType
);




// ====================
// === AuthProvider ===
// ====================

interface AuthProviderProps {
    runningOnDesktop: boolean;
    onAuthenticated: () => void;
    children: ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const AuthProvider = (props: AuthProviderProps): JSX.Element => {
  const { runningOnDesktop, onAuthenticated, children } = props
  const navigate = useNavigate();
  const [initialized, setInitialized] = useState(false);
  const [session, setSession] = useState<UserSession | undefined>(undefined);
  // State that, when incremented, forces a refresh of the user session. This is useful when a user
  // has just logged in (so their cached credentials are out of date).
  const [refresh, setRefresh] = useState(0);
  const [auth] = useState(authApi());

  // FIXME [NP]: remove
  //// Ensure the AWS Amplify library is loaded & configured prior to providing the `AuthContext` to
  //// the rest of the app. The AWS Amplify library must be configured before anything else happens
  //// because until that is done, we cannot check session status, authenticate users, etc.
  //useEffect(() => {
  //  // FIXME [NP]: remove this log
  //  console.log("EFFECT: initialized");

  //  setInitialized(true);
  //}, [])

  // Fetch the JWT access token from the session via the AWS Amplify library.
  //
  // When invoked, retrieves the access token (if available) from the storage method chosen when
  // Amplify was configured (e.g. local storage). If the token is not available, returns
  // `undefined`.  If the token has expired, automatically refreshes the token and returns the new
  // token.
  useEffect(() => {
      // FIXME [NP]: remove this log
      console.log("EFFECT: fetchSession 0");

      const fetchSession = async () => {
        // FIXME [NP]: remove this log
        console.log("EFFECT: fetchSession");

        // FIXME [NP]: rename this
        const authSession = await auth.userSession();
        if (!authSession) {
          setInitialized(true)
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

          // FIXME [NP]: is this the correct place to do this?
          onAuthenticated()
        }

        // FIXME [NP]: remove this
        console.log("EFFECT: fetchSession: session", session)
        setSession(session)
        setInitialized(true)
      };

      // `Auth.currentSession()` throws an error if the user is not signed in. If the user isn't
      // signed in, we can't get the token. For our purposes, we don't care why *exactly* we
      // couldn't get the token, so we catch the error and return `undefined` instead.
      fetchSession()
        .catch((error) => {
          // FIXME [NP]: catch error and log it properly
          // FIXME [NP]: remove eslint disable
          // eslint-disable-next-line @typescript-eslint/no-unsafe-argument, @typescript-eslint/no-unsafe-member-access
          toast.error(error.message);
          console.log(error)
        });
  }, [refresh])

  const signUp = async (username: string, password: string) => {
      const params: SignUpParams = {
        username,
        password,
        attributes: {
          email: username,
          // eslint-disable-next-line @typescript-eslint/naming-convention
          "custom:fromDesktop": runningOnDesktop ? "true" : "false"
        }
      }

      const loading = toast.loading("Please wait...");
      try {
        await Auth.signUp(params)
      } catch (error: any) {
          if (isAmplifyError(error)) {
            if (error.code === "InvalidPasswordException") {
              toast.error(error.message)
              return
            } else if (error.code === "UsernameExistsException") {
              toast.error(error.message)
              return
            }
          }
          throw error
      } finally {
        toast.dismiss(loading)
      }

      toast.success("We have sent you an email with further instructions!")
  };

  const confirmSignUp = async (email: string, code: string) => {
    const loading = toast.loading("Please wait...");
    try {
      // FIXME [NP]: ensure we handle the error appropriately.
      await Auth.confirmSignUp(email, code)
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "NotAuthorizedException") {
          if (error.message === "User cannot be confirmed. Current status is CONFIRMED") {
            toast.success("Your account has already been confirmed! Please log in.")
            navigate(LOGIN_PATH)
            return
          }
        }
      }
    }finally {
      toast.dismiss(loading)
    }

    toast.success("Your account has been confirmed! Please log in.")
    navigate(LOGIN_PATH)
  }

  const setUsername = async (accessToken: string, username: string, email: string) => {
      const body: SetUsernameBody = { userName: username, userEmail: email };
      await api.setUsername(accessToken, body);
      toast.success("Your username has been set!")
      navigate(DASHBOARD_PATH);
  };

  const signInWithGoogle = async () => {
    await Auth.federatedSignIn({ provider: CognitoHostedUIIdentityProvider.Google })
  };

  const signInWithGitHub = async () => {
    await Auth.federatedSignIn({ customProvider: githubProvider });
  };

  const signInWithPassword = async (
    email: string,
    password: string,
  ) => {
    const loading = toast.loading("Please wait...");
    try {
      await Auth.signIn(email, password);
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "UserNotFoundException") {
          navigate(REGISTRATION_PATH);
          toast.error("User not found. Please register first.")
          return
        } else if (error.code === "UserNotConfirmedException") {
          toast.error("User not confirmed. Please check your email for a confirmation link.")
          return
        } else if (error.code === "NotAuthorizedException") {
          toast.error("Incorrect username or password.")
          return
        }
      }

      throw error
    } finally {
      toast.dismiss(loading);
    }

    // Now that we've logged in, we need to refresh the session so that the user's organization
    // information is available.
    setRefresh(refresh + 1);

    toast.success("Successfully logged in!")    
    navigate(DASHBOARD_PATH)
  };

  const forgotPassword = async (email: string) => {
    const loading = toast.loading("Please wait...");
    try {
      await Auth.forgotPassword(email);
    } catch (error) {
      if (isAmplifyError(error)) {
        if (error.code === "UserNotFoundException") {
          toast.error("User not found. Please register first.")
          return
        }
      }

      throw error
    } finally {
      toast.dismiss(loading);
    }

    toast.success("We have sent you an email with further instructions!")
    navigate(RESET_PASSWORD_PATH)
  }

  const resetPassword = async (email: string, code: string, password: string) => {
    const loading = toast.loading("Please wait...");
    try {
      await Auth.forgotPasswordSubmit(email, code, password)
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
    } finally {
      toast.dismiss(loading);
    }

    toast.success("Successfully reset password!")    
    navigate(LOGIN_PATH)
  };

  const signOut = async () => {
    await Auth.signOut();

    // Now that we've logged in, we need to refresh the session so that the user's token is cleared.
    setRefresh(refresh + 1);

    toast.success("Successfully logged out!")
    navigate(LOGIN_PATH)
  }

  const value = {
    signUp,
    confirmSignUp,
    setUsername,
    signInWithGoogle,
    signInWithGitHub,
    signInWithPassword,
    forgotPassword,
    resetPassword,
    signOut,
    session,
  };

  return (
        //{/* If the user is not logged in, redirect them to the login page. */}
        //{initialized && !session && <Navigate to={LOGIN_PATH} />}
        //{/* If the user is logged in, but has not set a username, redirect them to the set username page. */}
        //{initialized && session && session.state === "partial" && <Navigate to={SET_USERNAME_PATH} state={session} />}
        //{/* Only render the underlying app after we assert for the presence of a current user. */}
        //{initialized && session && session.state === "full" && children}
        //{/* FIXME [NP]: show an error page or make this unreachable? */}
    <AuthContext.Provider value={value}>
        {/* Only render the underlying app after we assert for the presence of a current user. */}
        {initialized && children}
        {/* FIXME [NP]: show an error page or make this unreachable? */}
    </AuthContext.Provider>
  )
};



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
