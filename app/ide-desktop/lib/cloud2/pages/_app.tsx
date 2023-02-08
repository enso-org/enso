import "../styles/globals.css";
import type { AppProps } from "next/app";

import { Auth } from "aws-amplify";
import { amplifyConfig } from "../config/auth";
import {
  AuthContext,
  getAccessTokenAndEmail,
  getOrganization,
  signOut,
} from "../service";
import React, { useEffect, useState } from "react";
import { unstable_batchedUpdates as batchedUpdate } from "react-dom";
import { Toaster } from "react-hot-toast";

Auth.configure(amplifyConfig);

export default function App({ Component, pageProps, router }: AppProps) {
  const [accessToken, setAccessToken] = useState<string | undefined>(undefined);
  const [organizationId, setOrganizationId] = useState<string | undefined>("");
  const [userEmail, setUserEmail] = useState<string | undefined>("");

  useEffect(() => {
    void (async (): Promise<void> => {
      const payload = await getAccessTokenAndEmail();

      const jwt = payload !== undefined ? payload.accessToken : undefined;
      const userEmail = payload !== undefined ? payload.userEmail : undefined;
      const organization = await getOrganization(jwt);

      if (accessToken && !organization.id) {
        await router.push("/set-username");
      }

      batchedUpdate(() => {
        setAccessToken(jwt);
        setUserEmail(userEmail);
        setOrganizationId(organization.id);
      });
    })();
  }, [accessToken]);

  const value = { accessToken, organizationId, signOut, userEmail };

  if (accessToken) {
    return (
      <AuthContext.Provider value={value}>
        <Component {...pageProps} />
        <Toaster position="top-center" reverseOrder={false} />
      </AuthContext.Provider>
    );
  } else if (Component.isAuthComponent === true) {
    return (
      <>
        <Component {...pageProps} />
        <Toaster position="top-center" reverseOrder={false} />
      </>
    );
  }
}
