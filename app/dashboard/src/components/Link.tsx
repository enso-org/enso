/** @file A styled colored link with an icon. */
import * as React from 'react'

import * as router from 'react-router-dom'
import * as toastify from 'react-toastify'

import * as focusHooks from '#/hooks/focusHooks'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

// ============
// === Link ===
// ============

/** Props for a {@link Link}. */
export interface LinkProps {
  readonly openInBrowser?: boolean
  readonly to: string
  readonly icon: string
  readonly text: string
}

/** A styled colored link with an icon. */
export default function Link(props: LinkProps) {
  const { openInBrowser = false, to, icon, text } = props
  const { getText } = textProvider.useText()
  const focusChildProps = focusHooks.useFocusChild()
  const className =
    'flex items-center gap-auth-link rounded-full px-auth-link-x py-auth-link-y text-center text-xs font-bold text-blue-500 transition-all duration-auth hover:text-blue-700 focus:text-blue-700'

  const contents = (
    <>
      <SvgMask src={icon} />
      {text}
    </>
  )

  return (
    <FocusRing>
      {openInBrowser ? (
        <aria.Link
          {...aria.mergeProps<aria.LinkProps>()(focusChildProps, {
            href: to,
            className,
            target: '_blank',
            onClick: () => {
              toastify.toast.success(getText('openedLinkInBrowser'))
            },
          })}
        >
          {contents}
        </aria.Link>
      ) : (
        <router.Link
          {...aria.mergeProps<router.LinkProps>()(focusChildProps, {
            to,
            className,
            target: '',
          })}
        >
          {contents}
        </router.Link>
      )}
    </FocusRing>
  )
}
