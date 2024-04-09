/** @file A styled colored link with an icon. */
import * as React from 'react'

import * as router from 'react-router-dom'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
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
  const focusChildProps = focusHooks.useFocusChild()

  return (
    <FocusRing>
      <router.Link
        {...aria.mergeProps<router.LinkProps>()(focusChildProps, {
          to,
          className:
            'flex items-center gap-auth-link rounded-full px-auth-link-x py-auth-link-y text-center text-xs font-bold text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700',
        })}
      >
        <SvgMask src={icon} />
        {text}
      </router.Link>
    </FocusRing>
  )
}
