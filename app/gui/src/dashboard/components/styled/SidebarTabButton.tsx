/** @file A styled button representing a tab on a sidebar. */
import { Button, type ButtonProps } from '#/components/AriaComponents'
import { tv } from '#/utilities/tailwindVariants'

const SIDEBAR_TAB_BUTTON_STYLES = tv({
  base: 'z-1 font-medium',
  variants: {
    isActive: { true: 'bg-white opacity-100' },
  },
})

// ========================
// === SidebarTabButton ===
// ========================

/** Props for a {@link SidebarTabButton}. */
export interface SidebarTabButtonProps {
  readonly id: string
  readonly isDisabled?: boolean
  readonly autoFocus?: boolean
  /** When `true`, the button is not faded out even when not hovered. */
  readonly isActive?: boolean
  readonly icon: string
  readonly label: string
  readonly onPress: ButtonProps['onPress']
}

/** A styled button representing a tab on a sidebar. */
export default function SidebarTabButton(props: SidebarTabButtonProps) {
  const { isDisabled = false, isActive = false, icon, label, onPress } = props

  const styles = SIDEBAR_TAB_BUTTON_STYLES({ isActive })

  return (
    <Button
      icon={icon}
      variant="ghost-fading"
      loaderPosition="icon"
      size="medium"
      isDisabled={isDisabled}
      rounded="full"
      className={styles}
      onPress={onPress}
    >
      {label}
    </Button>
  )
}
