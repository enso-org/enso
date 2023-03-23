/** @file Main dashboard component, responsible for listing user's projects as well as other
 * interactive components. */

import Templates from './templates'

// =================
// === Dashboard ===
// =================

function Dashboard() {
    const onTemplateClick = (name: string) => {
        console.log('name: %o', name)
    }
    return (
        <>
            {/* These are placeholders. When implementing a feature,
             * please replace the appropriate placeholder with the actual element.*/}
            <div id="header" />
            <Templates onTemplateClick={onTemplateClick} />
            <div id="drive-header" />
            <div id="directory-listing" />
        </>
    )
}

export default Dashboard
