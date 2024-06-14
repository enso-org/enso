/** @file Styled content of a settings tab. */
import * as React from 'react'

// ==========================
// === SettingsTabContent ===
// ==========================

/** Props for a {@link SettingsPage}. */
export interface SettingsPageProps extends Readonly<React.PropsWithChildren> {}

/** Styled content of a settings tab. */
export default function SettingsPage(props: SettingsPageProps) {
  const { children } = props

  return (
    <div className="flex min-h-full flex-1 flex-col gap-settings-subsection overflow-auto">
      {children}
    </div>
  )
}
