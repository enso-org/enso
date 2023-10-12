/** @file Styled wrapper around FontAwesome icons. */
import * as React from 'react'

import type * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'
import * as fontawesome from '@fortawesome/react-fontawesome'

// =======================
// === FontAwesomeIcon ===
// =======================

/** Props for a {@link FontAwesomeIcon}. */
export interface FontAwesomeIconProps {
    icon: fontawesomeIcons.IconDefinition
}

/** A fixed-size container for a {@link fontawesome.FontAwesomeIcon FontAwesomeIcon}. */
export default function FontAwesomeIcon(props: FontAwesomeIconProps) {
    return (
        <span className="absolute left-0 top-0 flex h-full w-10 items-center justify-center text-blue-500">
            <fontawesome.FontAwesomeIcon icon={props.icon} />
        </span>
    )
}
