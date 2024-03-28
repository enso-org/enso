/** @file A link without an icon. */
import * as router from 'react-router-dom'

import * as focusHooks from '#/hooks/focusHooks'

import * as focusDirectionProvider from '#/providers/FocusDirectionProvider'

import FocusRing from '#/components/styled/FocusRing'

// ================
// === TextLink ===
// ================

/** Props for a {@link TextLink}. */
export interface TextLinkProps {
  readonly to: string
  readonly text: string
}

/** A link without an icon. */
export default function TextLink(props: TextLinkProps) {
  const { to, text } = props
  const focusDirection = focusDirectionProvider.useFocusDirection()
  const handleFocusMove = focusHooks.useHandleFocusMove(focusDirection)

  return (
    <FocusRing>
      <router.Link
        to={to}
        className="focus-child -mx-text-link-px self-end rounded-full px-text-link-x text-end text-xs text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700"
        onKeyDown={handleFocusMove}
      >
        {text}
      </router.Link>
    </FocusRing>
  )
}
