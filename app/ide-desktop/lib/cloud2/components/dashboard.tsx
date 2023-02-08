import * as React from "react";

import { NavBar } from "./navBar";
import { useAuth } from "../service";

export const Dashboard: React.FC<any> = () => {
  const { accessToken, userEmail, organizationId } = useAuth();

  return (
    <>
      <NavBar />
      <h1>{accessToken}</h1>
      <h1>{userEmail}</h1>
      <h1>{organizationId}</h1>
    </>
  );
};
