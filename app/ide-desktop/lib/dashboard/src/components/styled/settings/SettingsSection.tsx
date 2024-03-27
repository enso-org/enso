/** @file A styled settings section. */
import * as React from 'react'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'

// =======================
// === SettingsSection ===
// =======================

/** Props for a {@link SettingsSection}. */
export interface SettingsSectionProps extends Readonly<React.PropsWithChildren> {
  readonly title: React.ReactNode
  /** If `true`, the component is not wrapped in an {@link FocusArea}. */
  readonly noFocusArea?: boolean
  readonly className?: string
}

/** A styled settings section. */
export default function SettingsSection(props: SettingsSectionProps) {
  const { title, noFocusArea = false, className, children } = props
  const heading = (
    <aria.Heading level={2} className="settings-subheading h-[2.375rem] py-0.5 text-xl font-bold">
      {title}
    </aria.Heading>
  )

  return noFocusArea ? (
    <div className={`flex flex-col gap-settings-section-header ${className}`}>
      {heading}
      {children}
    </div>
  ) : (
    <FocusArea direction="vertical">
      {(ref, innerProps) => (
        <div
          ref={ref}
          className={`flex flex-col gap-settings-section-header ${className}`}
          {...innerProps}
        >
          {heading}
          {children}
        </div>
      )}
    </FocusArea>
  )
}
