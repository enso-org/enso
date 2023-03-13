/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */

import * as auth from "../../authentication/providers/auth";
import withRouter from "../../navigation";

// =================
// === Dashboard ===
// =================

const Dashboard = () => {
  const { accessToken } = auth.useFullUserSession();
  return (
    <>
      <h1>Hello dummy cloud dashboard</h1>
      <p>Access token: {accessToken}</p>
    </>
  );
};

export default withRouter(Dashboard);
