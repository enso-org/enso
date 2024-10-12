/** @file Rendering for an {@link settingsData.SettingsCustomEntryData}. */
import type * as settingsData from './data'

// ===========================
// === SettingsCustomEntry ===
// ===========================

/** Props for a {@link SettingsCustomEntry}. */
export interface SettingsCustomEntryProps {
  readonly context: settingsData.SettingsContext
  readonly data: settingsData.SettingsCustomEntryData
}

/** Rendering for an {@link settingsData.SettingsCustomEntryData}. */
export default function SettingsCustomEntry(props: SettingsCustomEntryProps) {
  const { context, data } = props
  const { render: Render, getVisible } = data
  const visible = getVisible?.(context) ?? true

  return !visible ? null : <Render {...context} />
}
