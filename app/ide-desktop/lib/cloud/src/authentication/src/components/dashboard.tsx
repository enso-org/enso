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
    const { signOut } = useAuth();
    const { email } = useFullUserSession();

    return (
        <div>
            <h1>Welcome to Enso Cloud {email}</h1>
            <a onClick={signOut}>Sign Out</a>
        </div>
    )
}

export default withRouter(dashboardContainer)
