/** @file Rendering for an {@link SettingsCustomEntryData}. */
import type { SettingsContext, SettingsCustomEntryData } from '#/layouts/Settings/settingsData'

// ==========================
// === SettingsCustomEntry ===
// ==========================

/** Props for a {@link SettingsCustomEntry}. */
export interface SettingsCustomEntryProps {
  readonly context: SettingsContext
  readonly data: SettingsCustomEntryData
}

/** Rendering for an {@link SettingsCustomEntryData}. */
export function SettingsCustomEntry(props: SettingsCustomEntryProps) {
  const { context, data } = props
  const { render: Render, getVisible } = data
  const visible = getVisible?.(context) ?? true

  return !visible ? null : <Render {...context} />
}
