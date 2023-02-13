/**
 * @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components.
 */

import * as React from 'react'

import withRouter from '../navigation'



// ==========================
// === dashboardContainer ===
// ==========================

const dashboardContainer: React.FC<any> = () => {
    return (
        <div>
            <h1>Hello Enso Cloud</h1>
        </div>
    )
}

export default withRouter(dashboardContainer)
