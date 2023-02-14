/**
 * @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components.
 */

import * as React from 'react'
import { useLocation, useNavigate } from 'react-router-dom';
import { useAuth } from '../authentication';
import { toast } from 'react-hot-toast'

import withRouter from '../navigation'
import { LOGIN_PATH } from './app';



// ==========================
// === dashboardContainer ===
// ==========================

const dashboardContainer: React.FC<any> = () => {
    const { session } = useAuth();
    const navigate = useNavigate();
    const location = useLocation();

    // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
    //logger.log("ERROR: session", session)
    toast.success(`session ${JSON.stringify(session)}`)
    if (!session) {
        // eslint-disable-next-line @typescript-eslint/no-unsafe-member-access, @typescript-eslint/no-unsafe-call
        //logger.log("navigate")
        //navigate(LOGIN_PATH)
        location.pathname = LOGIN_PATH
        toast.success("navigate login")
    }

    return (
        <div>
            <h1>Hello Enso Cloud</h1>
        </div>
    )
}

export default withRouter(dashboardContainer)
