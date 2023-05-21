/** @file Styled wrapper around FontAwesome icons. */
import * as React from 'react'

import * as fontawesome from '@fortawesome/react-fontawesome'
import * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'

// =======================
// === FontAwesomeIcon ===
// =======================

/** Props for a {@link FontAwesomeIcon}. */
export interface FontAwesomeIconProps {
    icon: fontawesomeIcons.IconDefinition
}

/** A fixed-size container for a {@link fontawesome.FontAwesomeIcon FontAwesomeIcon}. */
function FontAwesomeIcon(props: FontAwesomeIconProps) {
    return (
        <span
            className={
                'absolute left-0 top-0 flex items-center justify-center h-full w-10 ' +
                'text-blue-500'
            }
        >
            <fontawesome.FontAwesomeIcon icon={props.icon} />
        </span>
    )
}

export default FontAwesomeIcon
