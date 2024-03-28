/** @file A styled colored link with an icon. */
import * as React from 'react'

import * as router from 'react-router-dom'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import FocusRing from '#/components/styled/FocusRing'
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
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <FocusRing>
      <router.Link
        to={to}
        className="focus-child flex items-center gap-auth-link rounded-full px-auth-link-x py-auth-link-y text-center text-xs font-bold text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700"
        onKeyDown={handleFocusMove}
      >
        <SvgMask src={icon} />
        {text}
      </router.Link>
    </FocusRing>
  )
}
