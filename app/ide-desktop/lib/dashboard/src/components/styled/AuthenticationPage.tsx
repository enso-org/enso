/** @file A styled authentication page. */
import * as React from 'react'

import FocusArea from '#/components/styled/FocusArea'

// ==========================
// === AuthenticationPage ===
// ==========================

/** Props for an {@link AuthenticationPage}. */
export interface AuthenticationPageProps extends Readonly<React.PropsWithChildren> {
  readonly title: string
  readonly footer?: React.ReactNode
}

/** A styled authentication page. */
export default function AuthenticationPage(props: AuthenticationPageProps) {
  const { title, children, footer } = props

  return (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div
          ref={ref}
          className="flex min-h-screen flex-col items-center justify-center gap-auth text-sm text-primary"
          {...innerProps}
        >
          <div className="flex w-full max-w-md flex-col gap-auth rounded-auth bg-selected-frame p-auth shadow-md">
            <div className="self-center text-xl font-medium">{title}</div>
            {children}
          </div>
          {footer}
        </div>
      )}
    </FocusArea>
  )
}
