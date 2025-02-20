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

import { useText } from '#/providers/TextProvider'
import * as paywallDialog from './PaywallDialog'

/** Props for {@link ContextMenuEntry}. */
export interface ContextMenuEntryProps
  extends Omit<contextMenuEntry.ContextMenuEntryProps, 'isDisabled'> {
  readonly feature: billingHooks.PaywallFeatureName
  readonly isUnderPaywall: boolean
}

/** A context menu entry that opens a paywall dialog. */
export function ContextMenuEntry(props: ContextMenuEntryProps) {
  const { feature, isUnderPaywall, doAction, icon, ...rest } = props
  const { setModal } = modalProvider.useSetModal()
  const { getText } = useText()

  return (
    <ContextMenuEntryBase
      {...rest}
      icon={isUnderPaywall ? LockIcon : icon}
      tooltip={isUnderPaywall ? getText('upgradeToUseCloud') : null}
      doAction={() => {
        if (isUnderPaywall) {
          setModal(
            <paywallDialog.PaywallDialog modalProps={{ defaultOpen: true }} feature={feature} />,
          )
        } else {
          doAction()
        }
      }}
    />
  )
}
