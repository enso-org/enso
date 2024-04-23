/**
 * @file A component for displaying the result of an operation.
 */
import * as React from 'react'

import * as tw from 'tailwind-merge'

import Success from 'enso-assets/check_mark.svg'
import Error from 'enso-assets/cross.svg'

import SvgMask from '#/components/SvgMask'

import * as aria from './aria'

/**
 * The possible statuses for a result.
 */
export type Status = 'error' | 'success'

/**
 * The props for the Result component.
 */
export interface ResultProps extends React.PropsWithChildren {
  /**
   * The class name for the component.
   */
  readonly className?: string
  readonly title?: React.JSX.Element | string
  readonly subtitle?: React.JSX.Element | string
  /**
   * The status of the result.
   * @default 'success'
   */
  readonly status?: React.ReactElement | Status
}

/**
 * A component for displaying the result of an operation.
 */
export function Result(props: ResultProps) {
  const { title, children, status = 'success', subtitle, className } = props

  const statusIcon = typeof status === 'string' ? STATUS_ICON_MAP[status] : null

  return (
    <section
      className={tw.twMerge(
        'flex flex-col items-center justify-center px-6 py-4 text-center',
        className
      )}
    >
      {statusIcon != null ? (
        <div className="mb-4 flex rounded-full bg-green bg-opacity-25 p-1 text-green">
          <SvgMask
            src={statusIcon.icon}
            className={tw.twJoin('h-16 w-16 flex-none', statusIcon.colorClassName)}
          />
        </div>
      ) : (
        status
      )}
      {typeof title === 'string' ? (
        <aria.Heading level={2} className="text-3xl">
          {title}
        </aria.Heading>
      ) : (
        title
      )}
      {typeof subtitle === 'string' ? (
        <aria.Text elementType="p" className="mt-2 text-xl">
          {subtitle}
        </aria.Text>
      ) : (
        subtitle
      )}

      <div className="mt-6">{children}</div>
    </section>
  )
}

/**
 * The icon and color for each status.
 */
interface StatusIcon {
  readonly icon: string
  readonly colorClassName: string
}

const STATUS_ICON_MAP: Record<Status, StatusIcon> = {
  error: { icon: Error, colorClassName: 'text-red-500' },
  success: { icon: Success, colorClassName: 'text-green-500' },
}
