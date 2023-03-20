/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */

import * as auth from "../../authentication/providers/auth";

// =================
// === Dashboard ===
// =================

function Dashboard() {
  const { accessToken } = auth.useFullUserSession();
  return (
    <>
      <h1>This is a placeholder page for the cloud dashboard.</h1>
      <p>Access token: {accessToken}</p>
    </>
  );
}

export default Dashboard;
