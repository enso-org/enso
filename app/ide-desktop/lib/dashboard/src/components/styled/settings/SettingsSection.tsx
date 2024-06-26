/** @file A styled settings section. */
import * as React from 'react'

import * as ariaComponents from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'

import * as tailwindMerge from '#/utilities/tailwindMerge'

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

  const heading = <ariaComponents.Text.Heading level={2}>{title}</ariaComponents.Text.Heading>

  return noFocusArea ? (
    <div className={tailwindMerge.twMerge('flex flex-col gap-settings-section-header', className)}>
      {heading}
      {children}
    </div>
  ) : (
    <FocusArea direction="vertical">
      {innerProps => (
        <div
          className={tailwindMerge.twMerge('flex flex-col gap-settings-section-header', className)}
          {...innerProps}
        >
          {heading}
          {children}
        </div>
      )}
    </FocusArea>
  )
}
