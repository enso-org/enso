/** @file Alert component. */
import SvgMask from '#/components/SvgMask'
import { forwardRef } from '#/utilities/react'
import type { VariantProps } from '#/utilities/tailwindVariants'
import { type ForwardedRef, type HTMLAttributes, type PropsWithChildren } from 'react'
import { ALERT_STYLES } from './variants'

/** Props for an {@link Alert}. */
export interface AlertProps
  extends PropsWithChildren,
    VariantProps<typeof ALERT_STYLES>,
    HTMLAttributes<HTMLDivElement> {
  /** The icon to display in the Alert */
  readonly icon?: React.ReactElement | string | null | undefined
}

/** Alert component. */
export const Alert = forwardRef(function Alert(
  props: AlertProps,
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
      {icon != null &&
        (() => {
          if (typeof icon === 'string') {
            return (
              <div className={classes.iconContainer()}>
                <SvgMask src={icon} />
              </div>
            )
          }
          return <div className={classes.iconContainer()}>{icon}</div>
        })()}

      <div className={classes.children()}>{children}</div>
    </div>
  )
})
