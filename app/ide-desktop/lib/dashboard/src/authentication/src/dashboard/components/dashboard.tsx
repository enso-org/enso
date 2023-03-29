/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */
import { useEffect } from 'react'
import * as hooks from '../../hooks'
import TopBar from './topBar'

// =================
// === Dashboard ===
// =================

function Dashboard() {
    // The input value of TopBar
    const [searchVal, bindSearchVal] = hooks.useInput('')

    // The purpose of this effect is to enable search action.
    useEffect(() => {
        return () => {}
    }, [searchVal])
    return (
        <>
            {/* These are placeholders. When implementing a feature,
             * please replace the appropriate placeholder with the actual element.*/}
            <TopBar bindSearchVal={bindSearchVal} />
            <div id="templates" />
            <div id="drive-header" />
            <div id="directory-listing" />
        </>
    )
}

export default Dashboard
