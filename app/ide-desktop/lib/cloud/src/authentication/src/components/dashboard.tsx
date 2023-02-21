/**
 * @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components.
 */

import * as React from 'react'
import { FC } from 'react'
import { useAuth, useFullUserSession } from '../authentication';

import withRouter from '../navigation'



// ==========================
// === dashboardContainer ===
// ==========================

const dashboardContainer: FC = () => {
    const { signOut, session } = useAuth();
    const { email, accessToken, organization } = useFullUserSession();

    return (
        <div>
            <h1>Welcome to Enso Cloud {email}</h1>
            <a onClick={signOut}>Sign Out</a>
            <p>{accessToken}</p>
            <p>{email}</p>
            <p>{organization.id}</p>
            <p>{organization.name}</p>
        </div>
    )
}

export default withRouter(dashboardContainer)
