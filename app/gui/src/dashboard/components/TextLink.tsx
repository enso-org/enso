/** @file A link without an icon. */
import * as router from 'react-router-dom'

import * as focusHooks from '#/hooks/focusHooks'

import * as aria from '#/components/aria'
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
  const focusChildProps = focusHooks.useFocusChild()

  return (
    <FocusRing>
      <router.Link
        {...aria.mergeProps<router.LinkProps>()(focusChildProps, {
          to,
          className:
            '-mx-text-link-px self-end rounded-full px-text-link-x text-end text-xs text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700',
        })}
      >
        {text}
      </router.Link>
    </FocusRing>
  )
}
