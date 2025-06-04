/** @file A text display with an icon. */
import { Icon } from '#/components/Icon'
import { Text, type TextProps } from '#/components/Text'
import type { IconProp } from '#/components/types'
import { VisualTooltip, type TooltipElementType } from '#/components/VisualTooltip'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

const ICON_DISPLAY_STYLES = tv({
  base: 'flex items-center gap-2 max-w-[14.5rem] min-w-4 px-[7px] border-0.5 border-transparent',
  slots: {
    visualTooltip: 'flex',
    icon: '-mb-0.5',
    // For some reason `min-w-0` is required for the ellipsis to appear.
    container: 'flex min-w-0',
    text: 'block truncate',
  },
  variants: {
    variant: {
      custom: '',
      link: 'inline-block px-0 py-0 rounded-sm text-primary/50 underline border-0',
      primary: 'bg-primary text-white',
      accent: 'bg-accent text-white',
      ghost: 'text-primary',
      submit: 'bg-invite text-white opacity-80',
      outline: 'border-0.5 rounded-full border-primary/20 text-primary px-1 mx-1',
    },
    align: {
      left: { container: 'mr-auto' },
      center: { container: 'mx-auto' },
      right: { container: 'ml-auto' },
    },
  },
  defaultVariants: {
    variant: 'custom',
    iconPosition: 'default',
    align: 'center',
  },
})

/** Props for an {@link IconDisplay}. */
export interface IconDisplayProps<IconType extends string>
  extends Omit<TextProps, 'children' | 'variant' | 'variants'>,
    VariantProps<typeof ICON_DISPLAY_STYLES> {
  readonly icon: IconProp<IconType>
  readonly children: TooltipElementType
}

/** A text display with an icon. */
export function IconDisplay<IconType extends string>(props: IconDisplayProps<IconType>) {
  const {
    icon,
    children,
    variant,
    variants = ICON_DISPLAY_STYLES,
    tooltip,
    className,
    ...textProps
  } = props

  const styles = variants({ variant, align: props.align })

  return (
    <div className={styles.base({ className })}>
      <VisualTooltip className={styles.visualTooltip()} tooltip={tooltip} tooltipPlacement="left">
        <Icon color={textProps.color} className={styles.icon()} size="medium" icon={icon} />
      </VisualTooltip>
      <div className={styles.container()}>
        <Text className={styles.text()} truncate="1" {...textProps} tooltip={children}>
          {children}
        </Text>
      </div>
    </div>
  )
}
