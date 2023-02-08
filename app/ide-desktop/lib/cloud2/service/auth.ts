import * as React from "react";

import { createContext, useContext, useState } from "react";
import { Mixpanel } from "./mixpanel";
import { Auth } from "aws-amplify";
import { AuthContextType, Session } from "./types";
import { getMyOrganization } from "./userService";

export const AuthContext = createContext<AuthContextType>(undefined!);

export const useAuth = () => {
  const context = useContext(AuthContext);
  if (!context) {
    throw new Error(
      "Hook `useAuth` must be used within a fully-initialized `AuthProvider`."
    );
  }
  return context;
};

export const useInput = (initialValue: string) => {
  const [value, setValue] = useState(initialValue);

  return {
    value,
    setValue,
    reset: () => setValue(""),
    bind: {
      value,
      onChange: (event: React.SyntheticEvent<Element, Event>) => {
        // @ts-ignore
        setValue(event.target.value);
      },
      required: true,
    },
  };
};

export const signOut = async () => {
  Mixpanel.client.track(Mixpanel.events.LOGOUT_SUCCESS);
  Mixpanel.client.unregister();
  await Auth.signOut();
};

// Wrapper function for the Amplify library's JWT access token retrieval function.
//
// When invoked, retrieves the access token (if available) from the storage method chosen when
// Amplify was configured (e.g. local storage). If the token is not available, returns `undefined`.
// If the token has expired, automatically refreshes the token and returns the new token.
export const getAccessTokenAndEmail = async (): Promise<
  Session | undefined
> => {
  // Note [currentSession catch]
  // =====================================
  // `Auth.currentSession()` throws an error if the user is not signed in. If the user isn't
  // signed in, as we can't get the token. For our purposes, we don't care why *exactly* we
  // couldn't get the token, so we catch the error and return `undefined` instead.
  try {
    const session = await Auth.currentSession();
    return {
      // @ts-ignore
      userEmail: session.idToken.payload.email,
      accessToken: session.getAccessToken().getJwtToken(),
    };
  } catch (error) {
    console.log(error);
  }
};

// Wrapper function for our `getOrganization` API endpoint.
//
// Returns the organization ID and organization user name, if the user is authenticated and has set
// a user name for their organization. Returns error if the user is authenticated but the request
export const getOrganization = async (accessToken: string | undefined) => {
  // fails. Returns `undefined` for all fields if the user is not authenticated.
  let organizationId: string | undefined,
    userName: string | undefined,
    userEmail: string | undefined;

  if (accessToken) {
    const result = await getMyOrganization(accessToken);
    if (result) {
      organizationId = result.id;
      userName = result.name;
      userEmail = result.userEmail;
    }
  }

  return {
    id: organizationId,
    name: userName,
    userEmail: userEmail,
  };
};
