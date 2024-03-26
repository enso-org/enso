/**
 * @file
 * This file is the page for restoring an account that has been deleted.
 */
import * as React from 'react'

import * as textProvider from '#/providers/TextProvider'

import * as ariaComponents from '#/components/AriaComponents'

/**
 * The props for the RestoreAccount component.
 */
export interface RestoreAccountProps {}

/**
 * This component is the page for restoring an account that has been deleted.
 */
export default function RestoreAccount(props: RestoreAccountProps) {
  const {} = props
  const { getText } = textProvider.useText()

  return (
    <div>
      <h1>{getText('restoreAccount')}</h1>
      <p>{getText('restoreAccountDescription')}</p>

      <ariaComponents.Button>Restore</ariaComponents.Button>
      <ariaComponents.Button>Log Out</ariaComponents.Button>
    </div>
  )
}
