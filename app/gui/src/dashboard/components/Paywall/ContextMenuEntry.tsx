/**
 * @file
 *
 * A context menu entry that opens a paywall dialog.
 */

import * as React from 'react'

import LockIcon from '#/assets/lock.svg'

import type * as billingHooks from '#/hooks/billing'

import * as modalProvider from '#/providers/ModalProvider'

import type * as contextMenuEntry from '#/components/ContextMenuEntry'
import ContextMenuEntryBase from '#/components/ContextMenuEntry'

import * as paywallDialog from './PaywallDialog'

/** Props for {@link ContextMenuEntry}. */
export interface ContextMenuEntryProps
  extends Omit<contextMenuEntry.ContextMenuEntryProps, 'doAction' | 'isDisabled'> {
  readonly feature: billingHooks.PaywallFeatureName
}

/** A context menu entry that opens a paywall dialog. */
export function ContextMenuEntry(props: ContextMenuEntryProps) {
  const { feature, ...rest } = props
  const { setModal } = modalProvider.useSetModal()

  return (
    <>
      <ContextMenuEntryBase
        {...rest}
        icon={LockIcon}
        doAction={() => {
          setModal(
            <paywallDialog.PaywallDialog modalProps={{ defaultOpen: true }} feature={feature} />,
          )
        }}
      />
    </>
  )
}
