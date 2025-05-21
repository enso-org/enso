/** @file Alert component. */
import { Icon } from '#/components/Icon'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { forwardRef, type ForwardedRef, type HTMLAttributes, type PropsWithChildren } from 'react'
import type { IconProp } from '../types'
import { ALERT_STYLES } from './variants'

/** Props for an {@link Alert}. */
export interface AlertProps<IconType extends string = string>
  extends PropsWithChildren,
    VariantProps<typeof ALERT_STYLES>,
    HTMLAttributes<HTMLDivElement> {
  /** The icon to display in the Alert */
  readonly icon?: IconProp<IconType> | null | undefined
}

/** Alert component. */
// Use an explicit type assertion so that it plays nice with non-JSX `React.createElement`.
// eslint-disable-next-line no-restricted-syntax
export const Alert = forwardRef(function Alert<IconType extends string = string>(
  props: AlertProps<IconType>,
  ref: ForwardedRef<HTMLDivElement>,
) {
  const {
    children,
    className,
    variant,
    size,
    rounded,
    fullWidth,
    icon,
    variants = ALERT_STYLES,
    tabIndex: rawTabIndex,
    role: rawRole,
    ...containerProps
  } = props

  const tabIndex = variant === 'error' ? -1 : rawTabIndex
  const role = variant === 'error' ? 'alert' : rawRole

  const classes = variants({
    variant,
    size,
    rounded,
    fullWidth,
  })

  return (
    <div
      className={classes.base({ className })}
      ref={ref}
      tabIndex={tabIndex}
      role={role}
      {...containerProps}
    >
      {icon != null && <Icon icon={icon} size="medium" className={classes.iconContainer()} />}

      <div className={classes.children()}>{children}</div>
    </div>
  )
}) as <IconType extends string = string>(
  props: AlertProps<IconType> & { ref?: ForwardedRef<HTMLDivElement> },
) => JSX.Element
