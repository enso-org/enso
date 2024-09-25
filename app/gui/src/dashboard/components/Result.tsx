/** @file Display the result of an operation. */
import * as React from 'react'

import Success from '#/assets/check_mark.svg'
import Error from '#/assets/cross.svg'

import * as ariaComponents from '#/components/AriaComponents'
import * as loader from '#/components/Loader'
import SvgMask from '#/components/SvgMask'
import { tv, type VariantProps } from '#/utilities/tailwindVariants'

// =================
// === Constants ===
// =================

const INFO_ICON = (
  // eslint-disable-next-line no-restricted-syntax
  <ariaComponents.Text variant="custom" className="pb-0.5 text-xl leading-[0]" aria-hidden>
    !
  </ariaComponents.Text>
)

const STATUS_ICON_MAP: Readonly<Record<Status, StatusIcon>> = {
  loading: {
    icon: <loader.Loader minHeight="h8" />,
    colorClassName: 'text-primary',
    bgClassName: 'bg-transparent',
  },
  info: {
    icon: INFO_ICON,
    colorClassName: 'text-primary',
    bgClassName: 'bg-primary/30',
  },
  error: { icon: Error, colorClassName: 'text-red-500', bgClassName: 'bg-red-500' },
  success: { icon: Success, colorClassName: 'text-green-500', bgClassName: 'bg-green' },
  // pending is the same as loading. Used for mutations.
  pending: {
    icon: <loader.Loader minHeight="h8" />,
    colorClassName: 'text-primary',
    bgClassName: 'bg-transparent',
  },
  // idle is the same as info. Used for mutations.
  idle: {
    icon: INFO_ICON,
    colorClassName: 'text-primary',
    bgClassName: 'bg-primary/30',
  },
}

const RESULT_STYLES = tv({
  base: 'flex flex-col items-center justify-center max-w-full px-6 py-4 text-center h-[max-content]',
  variants: {
    centered: {
      true: 'm-auto',
      false: '',
      horizontal: 'mx-auto',
      vertical: 'my-auto',
      all: 'm-auto',
      none: '',
    },
  },
  slots: {
    statusIcon:
      'mb-2 flex h-8 w-8 flex-none items-center justify-center rounded-full bg-opacity-25 p-1 text-green',
    icon: 'h-6 w-6 flex-none',
    title: '',
    subtitle: 'max-w-[750px]',
    content: 'mt-3 w-full',
  },
  defaultVariants: { centered: 'all' },
})

// ==============
// === Status ===
// ==============

/** Possible statuses for a result. */
export type Status = 'error' | 'idle' | 'info' | 'loading' | 'pending' | 'success'

// ==================
// === StatusIcon ===
// ==================

/** The corresponding icon and color for each status. */
interface StatusIcon {
  readonly icon: React.ReactElement | string
  readonly colorClassName: string
  readonly bgClassName: string
}

// ==============
// === Result ===
// ==============

/** Props for a {@link Result}. */
export interface ResultProps extends React.PropsWithChildren, VariantProps<typeof RESULT_STYLES> {
  readonly className?: string
  readonly title?: React.JSX.Element | string
  readonly subtitle?: React.JSX.Element | string
  /**
   * The status of the result.
   * @default 'success'
   */
  readonly status?: React.ReactElement | Status
  readonly icon?: string | false
  readonly testId?: string
}

/** Display the result of an operation. */
export function Result(props: ResultProps) {
  const {
    title,
    children,
    status = 'success',
    subtitle,
    className,
    icon,
    testId = 'Result',
    centered,
  } = props

  const statusIcon = typeof status === 'string' ? STATUS_ICON_MAP[status] : null
  const showIcon = icon !== false

  const classes = RESULT_STYLES({ centered })

  return (
    <section className={classes.base({ className })} data-testid={testId}>
      {showIcon ?
        <>
          {statusIcon != null ?
            <div className={classes.statusIcon({ className: statusIcon.bgClassName })}>
              {typeof statusIcon.icon === 'string' ?
                <SvgMask
                  src={icon ?? statusIcon.icon}
                  className={classes.icon({ className: statusIcon.colorClassName })}
                />
              : statusIcon.icon}
            </div>
          : status}
        </>
      : null}

      {typeof title === 'string' ?
        <ariaComponents.Text.Heading level={2} className={classes.title()} variant="subtitle">
          {title}
        </ariaComponents.Text.Heading>
      : title}

      {typeof subtitle === 'string' ?
        <ariaComponents.Text elementType="p" className={classes.subtitle()} balance variant="body">
          {subtitle}
        </ariaComponents.Text>
      : subtitle}

      {children != null && <div className={classes.content()}>{children}</div>}
    </section>
  )
}
