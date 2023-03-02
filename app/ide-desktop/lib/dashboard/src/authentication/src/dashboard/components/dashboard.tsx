/* eslint-disable @typescript-eslint/no-non-null-assertion */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/** @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components. */

import * as React from "react";

import * as auth from "../../authentication/providers/auth";
import withRouter from "../../navigation";



// ==========================
// === dashboardContainer ===
// ==========================

const dashboardContainer = () => {
    const { signOut } = auth.useAuth();
    const { accessToken, organization } = auth.useFullUserSession();

    return (
        <>
            <h1>Hello dummy cloud dashboard</h1>
            <p>Organization: {organization.id}</p>
            <p>Access token: {accessToken}</p>
            <button onClick={signOut}>Log out</button>
        </>
    )
}

export default withRouter(dashboardContainer);
