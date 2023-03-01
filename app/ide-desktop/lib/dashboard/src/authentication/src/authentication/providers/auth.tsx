/** @file Module for authenticating users with AWS Cognito.
 *
 * Provides an `AuthProvider` component that wraps the entire application, and a `useAuth` hook that
 * can be used from any React component to access the currently logged-in user's session data. The
 * hook also provides methods for registering a user, logging in, logging out, etc. */
import react from "react";
import * as router from "react-router-dom";
import toast from "react-hot-toast";

import * as backendService from "../../dashboard/service";
import * as authService from "../service";
import * as app from "../../components/app";
import * as loggerProvider from "../../providers/logger";
import * as sessionProvider from "./session";
import * as error from "../../error";



// =================
// === Constants ===
// =================

const MESSAGES = {
  signUpSuccess: "We have sent you an email with further instructions!",
  confirmSignUpSuccess: "Your account has been confirmed! Please log in.",
  setUsernameSuccess: "Your username has been set!",
  signInWithPasswordSuccess: "Successfully logged in!",
  forgotPasswordSuccess: "We have sent you an email with further instructions!",
  resetPasswordSuccess: "Successfully reset password!",
  signOutSuccess: "Successfully logged out!",
  pleaseWait: "Please wait...",
};



// =============
// === Types ===
// =============

// === UserSession ===

export type UserSession = FullUserSession | PartialUserSession;

/** Object containing the currently signed-in user's session data. */
export interface FullUserSession {
  /** A type-hint for TypeScript to be able to disambiguate between this interface and other
   * `UserSession` variants. */
  state: "full";
  /** User's JSON Web Token (JWT), used for authenticating and authorizing requests to the API. */
  accessToken: string;
  /** User's email address. */
  email: string;
  /** User's organization information. */
  organization: backendService.Organization;
}

/** Object containing the currently signed-in user's session data, if the user has not yet set their
 * username.
 *
 * If a user has not yet set their username, they do not yet have an organization associated with
 * their account. Otherwise, this type is identical to the `Session` type. This type should ONLY be
 * used by the `SetUsername` component. */
export interface PartialUserSession {
  /** A type-hint for TypeScript to be able to disambiguate between this interface and other
   * `UserSession` variants. */
  state: "partial";
  /** User's JSON Web Token (JWT), used for authenticating and authorizing requests to the API. */
  accessToken: string;
  /** User's email address. */
  email: string;
}



// ===================
// === AuthContext ===
// ===================

/** Interface returned by the `useAuth` hook.
 *
 * Contains the currently authenticated user's session data, as well as methods for signing in,
 * signing out, etc. All interactions with the authentication API should be done through this
 * interface.
 *
 * See {@link Cognito} for details on each of the authentication functions. */
interface AuthContextType {
  signUp: (email: string, password: string) => Promise<void>;
  confirmSignUp: (email: string, code: string) => Promise<void>;
  setUsername: (
    accessToken: string,
    username: string,
    email: string
  ) => Promise<void>;
  signInWithGoogle: () => Promise<null>;
  signInWithGitHub: () => Promise<null>;
  signInWithPassword: (email: string, password: string) => Promise<void>;
  forgotPassword: (email: string) => Promise<void>;
  resetPassword: (
    email: string,
    code: string,
    password: string
  ) => Promise<void>;
  signOut: () => Promise<void>;
  /** Session containing the currently authenticated user's authentication information.
   *
   * If the user has not signed in, the session will be `undefined`. */
  session: UserSession | undefined;
}

/** Create a global instance of the `AuthContextType`, that will be re-used between all React
 * components that use the `useAuth` hook.
 *
 * # Safety of Context Initialization
 *
 * An `as ...` cast is unsafe. We use this cast when creating the context. So it appears that the
 * `AuthContextType` can be unsafely (i.e., only partially) initialized as a result of this.
 *
 * So it appears that we should remove the cast and initialize the context as `undefined` instead.
 *
 * **However**, initializing a context the existing way is the recommended way to initialize a
 * context in React.  It is safe, for non-obvious reasons. It is safe because the `AuthContext` is
 * only accessible through the `useAuth` hook.
 *
 * 1. If the `useAuth` hook is called in a component that is a child of an `AuthProvider`, then the
 * context is guaranteed to be initialized, because the `AuthProvider` constructor is what
 * initializes it. So the cast is safe.
 * 2. If the `useAuth` hook is called in a component that is not a child of an `AuthProvider`, then
 * the hook will throw an error regardless, because React does not support using hooks outside of
 * their supporting providers.
 *
 * So changing the cast would provide no safety guarantees, and would require us to introduce null
 * checks everywhere we use the context. */
// eslint-disable-next-line @typescript-eslint/naming-convention
const AuthContext = react.createContext<AuthContextType>({} as AuthContextType);



// ====================
// === AuthProvider ===
// ====================

export interface AuthProviderProps {
  authService: authService.AuthService;
  /** Callback to execute once the user has authenticated successfully. */
  onAuthenticated: () => void;
  children: react.ReactNode;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
export const AuthProvider = (props: AuthProviderProps) => {
  const { authService, children } = props;
  const { cognito } = authService;
  const { session } = sessionProvider.useSession();
  const logger = loggerProvider.useLogger();
  const onAuthenticated = react.useCallback(props.onAuthenticated, []);
  const navigate = router.useNavigate();
  const [initialized, setInitialized] = react.useState(false);
  const [userSession, setUserSession] = react.useState<UserSession | undefined>(
    undefined
  );

  // Fetch the JWT access token from the session via the AWS Amplify library.
  //
  // When invoked, retrieves the access token (if available) from the storage method chosen when
  // Amplify was configured (e.g. local storage). If the token is not available, returns
  // `undefined`.  If the token has expired, automatically refreshes the token and returns the new
  // token.
  react.useEffect(() => {
    const fetchSession = async () => {
      if (session.none) {
        setInitialized(true);
        setUserSession(undefined);
        return;
      }
      const { accessToken, email } = session.val;

      const backend = backendService.createBackend(accessToken, logger);

      // Request the user's organization information from the Cloud backend.
      const organization = await backend.getUser();

      let userSession: UserSession;

      if (!organization) {
        userSession = {
          state: "partial",
          email,
          accessToken,
        };
      } else {
        userSession = {
          state: "full",
          email,
          accessToken,
          organization,
        };

        // Execute the callback that should inform the Electron app that the user has logged in.
        // This is done to transition the app from the authentication/dashboard view to the IDE.
        onAuthenticated();
      }

      setUserSession(userSession);
      setInitialized(true);
    };

    fetchSession().catch((error) => {
      if (isUserFacingError(error)) {
        toast.error(error.message);
      } else {
        logger.error(error);
      }
    });
  }, [session]);

  const withLoadingToast =
    <T extends any[]>(action: (...args: T) => Promise<void>) =>
    async (...args: T) => {
      const loadingToast = toast.loading(MESSAGES.pleaseWait);
      try {
        await action(...args);
      } finally {
        toast.dismiss(loadingToast);
      }
    };

  const signUp = (username: string, password: string) =>
    cognito.signUp(username, password).then((result) => {
      if (result.ok) {
        toast.success(MESSAGES.signUpSuccess);
      } else {
        toast.error(result.val.message);
      }
    });

  const confirmSignUp = async (email: string, code: string) =>
    cognito.confirmSignUp(email, code).then((result) => {
      if (result.err) {
        switch (result.val.kind) {
          case "UserAlreadyConfirmed":
            break;
          default:
            throw new error.UnreachableCaseError(result.val.kind);
        }
      }

      toast.success(MESSAGES.confirmSignUpSuccess);
      navigate(app.LOGIN_PATH);
    });

  const setUsername = async (
    accessToken: string,
    username: string,
    email: string
  ) => {
    const body: backendService.SetUsernameRequestBody = {
      userName: username,
      userEmail: email,
    };

    // FIXME [NP2]: don't create a new API client here, reuse the one from the context.
    const backend = backendService.createBackend(accessToken, logger);

    await backend.setUsername(body);
    navigate(app.DASHBOARD_PATH);
    toast.success(MESSAGES.setUsernameSuccess);
  };

  const signInWithPassword = async (email: string, password: string) =>
    cognito.signInWithPassword(email, password).then((result) => {
      if (result.ok) {
        toast.success(MESSAGES.signInWithPasswordSuccess);
        return;
      }

      if (result.val.kind === "UserNotFound") {
        navigate(app.REGISTRATION_PATH);
      }

      toast.error(result.val.message);
    });

  const forgotPassword = async (email: string) =>
    cognito.forgotPassword(email).then((result) => {
      if (result.ok) {
        toast.success(MESSAGES.forgotPasswordSuccess);
        navigate(app.RESET_PASSWORD_PATH);
      } else {
        toast.error(result.val.message);
      }
    });

  const resetPassword = async (email: string, code: string, password: string) =>
    cognito.forgotPasswordSubmit(email, code, password).then((result) => {
      if (result.ok) {
        toast.success(MESSAGES.resetPasswordSuccess);
        navigate(app.LOGIN_PATH);
      } else {
        toast.error(result.val.message);
      }
    });

  const signOut = () =>
    cognito
      .signOut()
      .then(() => toast.success(MESSAGES.signOutSuccess))
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
    session: userSession,
  };

  return (
    <AuthContext.Provider value={value}>
      {/* Only render the underlying app after we assert for the presence of a current user. */}
      {initialized && children}
    </AuthContext.Provider>
  );
};

/** Type of an error containing a `string`-typed `message` field.
 *
 * Many types of errors fall into this category. We use this type to check if an error can be safely
 * displayed to the user. */
interface UserFacingError {
  /** The user-facing error message. */
  message: string;
}

/** Returns `true` if the value is a {@link UserFacingError}. */
const isUserFacingError = (value: unknown): value is UserFacingError => {
  return typeof value === "object" && value !== null && "message" in value;
};



// ===============
// === useAuth ===
// ===============

/** A React hook that provides access to the authentication context.
 *
 * Only the hook is exported, and not the context, because we only want to use the hook directly
 * and never the context component. */
export const useAuth = () => react.useContext(AuthContext);



// =======================
// === ProtectedLayout ===
// =======================

// eslint-disable-next-line @typescript-eslint/naming-convention
export const ProtectedLayout = () => {
  const { session } = useAuth();

  if (!session) {
    return <router.Navigate to={app.LOGIN_PATH} />;
  }

  return <router.Outlet context={session} />;
};



// ===================
// === GuestLayout ===
// ===================

// eslint-disable-next-line @typescript-eslint/naming-convention
export const GuestLayout = () => {
  const { session } = useAuth();

  if (session?.state == "partial") {
    return <router.Navigate to={app.SET_USERNAME_PATH} />;
  }

  if (session?.state == "full") {
    return <router.Navigate to={app.DASHBOARD_PATH} />;
  }

  return <router.Outlet />;
};



// =============================
// === usePartialUserSession ===
// =============================

export const usePartialUserSession = () => {
  return router.useOutletContext<PartialUserSession>();
};



// ==========================
// === useFullUserSession ===
// ==========================

export const useFullUserSession = () => {
  return router.useOutletContext<FullUserSession>();
};
