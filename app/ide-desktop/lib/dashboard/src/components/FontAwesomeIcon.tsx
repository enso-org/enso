/** @file A fixed-size container for a {@link fontawesome.FontAwesomeIcon FontAwesomeIcon}. */
import * as React from 'react'

import type * as fontawesomeIcons from '@fortawesome/free-brands-svg-icons'
import * as fontawesome from '@fortawesome/react-fontawesome'

// =======================
// === FontAwesomeIcon ===
// =======================

/** Props for a {@link FontAwesomeIcon}. */
export interface FontAwesomeIconProps {
  readonly icon: fontawesomeIcons.IconDefinition
}

/** A fixed-size container for a {@link fontawesome.FontAwesomeIcon FontAwesomeIcon}. */
export default function FontAwesomeIcon(props: FontAwesomeIconProps) {
  return (
    <fontawesome.FontAwesomeIcon
      className="absolute left top h-full w-icon px-font-awesome-icon-x text-blue-500"
      icon={props.icon}
    />
  )
}
