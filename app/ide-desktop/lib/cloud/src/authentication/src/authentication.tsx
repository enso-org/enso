import { Auth, CognitoHostedUIIdentityProvider } from '@aws-amplify/auth'
import { createContext, ReactNode, useContext, useEffect, useState } from 'react';



// =============
// === Types ===
// =============

/// The type of the object consumed by the `Auth.configure` method.
type AuthOptions = ReturnType<typeof Auth['configure']>



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
const browserAmplifyConfigNpekin: AuthOptions = {
  region: "eu-west-1",
  // FIXME [NP]
  //identityPoolId: "",
  userPoolId: "eu-west-1_sP5bQ4mJs",
  userPoolWebClientId: "27gd0b05qlnkj1lcsnd0b4fb89",
  oauth: {
    options: {}, // FIXME [NP]
    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
    domain: "npekin-enso-domain.auth.eu-west-1.amazoncognito.com/",
    scope: ['email', 'openid'], // FIXME [NP]
    redirectSignIn: "http://localhost:8081",
    redirectSignOut: "http://localhost:8081",
    responseType: "code",
  },
}
const browserAmplifyConfigProd: AuthOptions = {
  region: "eu-west-1",
  // FIXME [NP]
  //identityPoolId: "",
  userPoolId: "eu-west-1_9Kycu2SbD",
  userPoolWebClientId: "4j9bfs8e7415erf82l129v0qhe",
  oauth: {
    options: {}, // FIXME [NP]
    //domain: "https://npekin-enso-domain.auth.eu-west-1.amazoncognito.com",
    domain: "production-enso-domain.auth.eu-west-1.amazoncognito.com/",
    scope: ['email', 'openid'], // FIXME [NP]
    redirectSignIn: "https://cloud.enso.org",
    redirectSignOut: "https://cloud.enso.org",
    responseType: "code",
  },
}
const amplifyConfig = browserAmplifyConfigNpekin;



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
    signUp: (email: string, password: string) => Promise<void>;
    signInWithGoogle: () => Promise<void>;
    signInWithGitHub: () => Promise<void>;
    signInWithPassword: (email: string, password: string) => Promise<void>;
}

// eslint-disable-next-line @typescript-eslint/naming-convention
const AuthContext = createContext<AuthContextType>(
    {} as AuthContextType
);




// ====================
// === AuthProvider ===
// ====================

// eslint-disable-next-line @typescript-eslint/naming-convention
const AuthProvider = ({ children }: { children: ReactNode }): JSX.Element => {
  const [initialized, setInitialized] = useState(false);

  /// Ensure the AWS Amplify library is loaded & configured prior to providing the `AuthContext` to
  /// the rest of the app. The AWS Amplify library must be configured before anything else happens
  /// because until that is done, we cannot check session status, authenticate users, etc.
  useEffect(() => {
    setInitialized(true);
  })

  const signUp = async (username: string, password: string) => {
    const email = username 
    const attributes = { email }
    const params = { username, password, attributes }
    await Auth.signUp(params)
  };
  const signInWithGoogle = async () => {
    await Auth.federatedSignIn({ provider: CognitoHostedUIIdentityProvider.Google })
  };
  const signInWithGitHub = async () => {
    await Auth.federatedSignIn({ customProvider: githubProvider });
  };
  const signInWithPassword = async (email: string, password: string) => {
    await Auth.signIn(email, password);
  };

  const value = {
    signUp,
    signInWithGoogle,
    signInWithGitHub,
    signInWithPassword,
  };

  return (
    <AuthContext.Provider value={value}>
        {/* Only render the underlying app after we assert for the presence of a current user. */}
        {initialized && children}
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
const useAuth = () => useContext(AuthContext);

// eslint-disable-next-line @typescript-eslint/naming-convention
export { AuthProvider, useAuth }
