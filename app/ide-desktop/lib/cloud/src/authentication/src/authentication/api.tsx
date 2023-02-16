import { Auth } from "@aws-amplify/auth";

/**
 * Returns the current `CognitoUserSession`, or `null` if the user is not logged in.
 *
 * Will refresh the session if it has expired.
 *
 * @returns `CognitoUserSession` if the user is logged in, `null` otherwise.
 */
const currentSession = async () => {
    try {
        return await Auth.currentSession();
    } catch (error) {
        // If the user is not logged in, `Auth.currentSession()` throws an error. We catch this
        // error and return `null` instead.
        if (error === "No current user") {
            return null;
        }

        throw error;
    }
}

/// The payload of a Cognito ID token.
interface Payload {
    /// User's email address.
    ///
    /// Provided by the identity provider the user used to log in. One of:
    ///
    /// - GitHub
    /// - Google
    /// - Email
    ///
    /// This field is mandatory. It is marked as optional because the `payload` field of
    /// `CognitoIdToken` is not typed, so TypeScript cannot infer that the `email` field is present.
    email?: string;
}

/// User's session.
interface UserSession {
    /// User's email address.
    ///
    /// Used to uniquely identify the user.
    email: string;
    /// User's access token.
    ///
    /// Used to authenticate the user (e.g., when making API calls).
    accessToken: string;
}

/**
 * Returns the current `UserSession`, or `null` if the user is not logged in.
 * 
 * Will refresh the session if it has expired. 
 *
 * @returns `UserSession` if the user is logged in, `null` otherwise.
 */
const userSession = async (): Promise<UserSession | null> => {
    const session = await currentSession();
    if (session == null) {
        return null;
    }

    const payload: Payload = session.getIdToken().payload;
    // The `email` field is mandatory, so we can safely use the non-null assertion operator here.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const email = payload.email!;
    const accessToken = session.getAccessToken().getJwtToken();

    return { email, accessToken };
}

const api = () => {
  return {
    userSession,
  }
}

export default api
