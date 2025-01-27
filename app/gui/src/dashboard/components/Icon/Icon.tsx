/**
 * @file
 *
 * Icon component that displays an icon based on different input.
 */
import { tv, type VariantProps } from '#/utilities/tailwindVariants'
import { memo } from 'react'
import type { IconProp as IconType, TestIdProps } from '../AriaComponents'
import SvgMask from '../SvgMask'

/**
 * Props for {@link Icon}.
 */
export interface IconProps<Render = never> extends VariantProps<typeof ICON_STYLES>, TestIdProps {
  readonly children: IconType<Render>
  readonly className?: string | undefined
  readonly renderProps?: Render
}

export const ICON_STYLES = tv({
  base: 'flex-none aspect-square [&>svg]:stroke-current [&>svg]:w-full [&>svg]:h-full',
  variants: {
    color: {
      custom: '',
      primary: 'text-primary',
      danger: 'text-danger',
      success: 'text-accent-dark',
      accent: 'text-accent-dark',
      muted: 'text-primary/40',
      disabled: 'text-disabled',
      invert: 'text-invert',
      inherit: 'text-inherit',
      current: 'text-current',
    },
    size: {
      xsmall: 'h-2 w-2',
      small: 'h-3 w-3',
      medium: 'h-4 w-4',
      large: 'h-5 w-5',
      xlarge: 'h-6 w-6',
      xxlarge: 'h-7 w-7',
      xxxlarge: 'h-8 w-8',
      xxxxlarge: 'h-9 w-9',
      full: 'h-full w-full',
    },
  },
  defaultVariants: {
    color: 'primary',
    size: 'medium',
  },
})

/**
 * Icon component that displays an icon based on different input.
 */
// eslint-disable-next-line no-restricted-syntax
export const Icon = memo(function Icon<Render = never>(props: IconProps<Render>) {
  const {
    children,
    className,
    variants = ICON_STYLES,
    size,
    testId,
    renderProps = {},
    color,
  } = props

  const styles = variants({
    size,
    className,
    color,
  })

  const actualIcon = (() => {
    // eslint-disable-next-line no-restricted-syntax
    const icon = typeof children === 'function' ? children(renderProps as Render) : children

    if (icon == null) {
      return null
    }

    return typeof icon === 'string' ?
        <SvgMask src={icon} className={styles} testId={testId} />
      : <span className={styles} data-testid={testId}>
          {icon}
        </span>
  })()

  return <>{actualIcon}</>
}) as <Render = never>(props: IconProps<Render>) => React.JSX.Element
