/** @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components. */

import * as auth from "../../authentication/providers/auth";
import withRouter from "../../navigation";

// ==========================
// === dashboardContainer ===
// ==========================

const dashboardContainer = () => {
  const { accessToken } = auth.useFullUserSession();
  return (
    <>
      <h1>Hello dummy cloud dashboard</h1>
      <p>Access token: {accessToken}</p>
    </>
  );
};

export default withRouter(dashboardContainer);
