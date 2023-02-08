/** @file Login container responsible for rendering and interactions in sign in flow. */

import * as React from 'react'

import withRouter from '../navigation/withRouter'



// ======================
// === loginContainer ===
// ======================

const loginContainer: React.FC<any> = () => {

    return (
        <div>
            <h1>Login</h1>
        </div>
    )
}

export default withRouter(loginContainer)
