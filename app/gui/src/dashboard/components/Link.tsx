/** @file A styled colored link with an icon. */
import * as React from 'react'
import * as toastify from 'react-toastify'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'
import { mergeRefs } from '#/utilities/mergeRefs'
import { useText } from '$/providers/react'

/** Props for a {@link Link}. */
export interface LinkProps {
  readonly onPress?: () => void
  readonly openInBrowser?: boolean
  readonly to: string
  readonly icon: string
  readonly text: string
}

export default React.forwardRef(Link)

/** A styled colored link with an icon. */
function Link(props: LinkProps, ref: React.ForwardedRef<HTMLAnchorElement>) {
  const { openInBrowser = false, to, icon, text, onPress } = props
  const { getText } = useText()
  const linkRef = React.useRef<HTMLAnchorElement>(null)

  return (
    <FocusRing>
      <aria.Link
        ref={(el) => {
          mergeRefs(linkRef, ref)(el)
        }}
        href={to}
        {...(openInBrowser && { target: '_blank' })}
        rel="noopener noreferrer"
        className="flex items-center gap-auth-link rounded-full px-auth-link-x py-auth-link-y text-center text-xs font-bold text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700"
        onPress={() => {
          if (openInBrowser) {
            toastify.toast.success(getText('openedLinkInBrowser'))
          }
          onPress?.()
        }}
      >
        <SvgMask src={icon} />
        {text}
      </aria.Link>
    </FocusRing>
  )
}
