/** @file A context menu entry that opens a paywall dialog. */
import LockIcon from '#/assets/lock.svg'
import type { ContextMenuEntryProps as ContextMenuEntryBaseProps } from '#/components/ContextMenuEntry'
import ContextMenuEntryBase from '#/components/ContextMenuEntry'
import type { PaywallFeatureName } from '#/hooks/billing'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import { PaywallDialog } from './PaywallDialog'

/** Props for {@link ContextMenuEntry}. */
export interface ContextMenuEntryProps extends Omit<ContextMenuEntryBaseProps, 'isDisabled'> {
  readonly feature: PaywallFeatureName
  readonly isUnderPaywall: boolean
}

/** A context menu entry that opens a paywall dialog. */
export function ContextMenuEntry(props: ContextMenuEntryProps) {
  const { feature, isUnderPaywall, doAction, icon, ...rest } = props
  const { setModal } = useSetModal()
  const { getText } = useText()

  return (
    <ContextMenuEntryBase
      {...rest}
      icon={isUnderPaywall ? LockIcon : icon}
      tooltip={isUnderPaywall ? getText('upgradeToUseCloud') : null}
      doAction={() => {
        if (isUnderPaywall) {
          setModal(<PaywallDialog modalProps={{ defaultOpen: true }} feature={feature} />)
        } else {
          doAction()
        }
      }}
    />
  )
}
