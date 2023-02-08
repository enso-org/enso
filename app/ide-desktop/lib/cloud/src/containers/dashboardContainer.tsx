/** @file Main dashboard container responsible for listing users projects as well as others interactive components. */

import * as React from 'react'

import withRouter from '../navigation/withRouter'



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
