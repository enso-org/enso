/** @file A fixed-size container for a {@link fontawesome.FontAwesomeIcon FontAwesomeIcon}. */
import * as React from 'react'

import * as fontawesome from '@fortawesome/react-fontawesome'
import type * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'

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
        <fontawesome.FontAwesomeIcon
            className="absolute left-0 top-0 text-blue-500 px-3 h-full w-4"
            icon={props.icon}
        />
    )
}
