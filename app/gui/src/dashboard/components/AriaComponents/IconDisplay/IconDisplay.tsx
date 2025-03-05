/** @file A text display with an icon. */
import { Icon } from '#/components/Icon'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import type { ReactNode } from 'react'
import { Text, type IconProp, type TextProps } from '..'

const ICON_DISPLAY_STYLES = tv({
  base: 'block max-w-48 min-w-4 w-auto',
  slots: {
    container: 'flex items-center gap-2',
    icon: '-mb-0.5',
  },
  variants: {
    variant: {
      custom: '',
      link: 'inline-block px-0 py-0 rounded-sm text-primary/50 underline border-0',
      primary: 'bg-primary text-white',
      accent: 'bg-accent text-white',
      ghost: 'text-primary',
      submit: 'bg-invite text-white opacity-80',
      outline: 'border-primary/20 text-primary',
    },
  },
})

/** Render props for {@link IconDisplay}. */
export interface IconDisplayRenderProps {
  /** Defaults to `true`. */
  readonly isCurrent?: boolean
  /** Defaults to `false`. */
  readonly isDisabled?: boolean
}

/** Props for an {@link IconDisplay}. */
export interface IconDisplayProps<IconType extends string>
  extends Omit<TextProps, 'children' | 'variant' | 'variants'>,
    IconDisplayRenderProps,
    VariantProps<typeof ICON_DISPLAY_STYLES> {
  readonly icon: IconProp<IconType, Required<IconDisplayRenderProps>>
  readonly children: ReactNode | ((renderProps: Required<IconDisplayRenderProps>) => ReactNode)
}

/** A text display with an icon. */
export function IconDisplay<IconType extends string>(props: IconDisplayProps<IconType>) {
  const {
    icon,
    isCurrent = true,
    isDisabled = false,
    children,
    variant,
    variants = ICON_DISPLAY_STYLES,
    ...textProps
  } = props
  const renderProps = { isCurrent, isDisabled }

  const styles = variants({ variant })

  return (
    <Text className={styles.base()} nowrap truncate="1" {...textProps}>
      <span className={styles.container()}>
        <Icon className={styles.icon()} size="medium" renderProps={renderProps}>
          {icon}
        </Icon>
        {typeof children === 'function' ? children(renderProps) : children}
      </span>
    </Text>
  )
}
