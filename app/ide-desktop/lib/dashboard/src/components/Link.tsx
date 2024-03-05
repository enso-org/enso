/** @file A styled colored link with an icon. */
import * as React from 'react'

import * as router from 'react-router-dom'

import SvgMask from '#/components/SvgMask'

// ============
// === Link ===
// ============

/** Props for a {@link Link}. */
export interface LinkProps {
  readonly to: string
  readonly icon: string
  readonly text: string
}

/** A styled colored link with an icon. */
export default function Link(props: LinkProps) {
  const { to, icon, text } = props
  return (
    <router.Link
      to={to}
      className="flex items-center gap-auth-link text-center text-xs font-bold text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700"
    >
      <SvgMask src={icon} />
      {text}
    </router.Link>
  )
}
