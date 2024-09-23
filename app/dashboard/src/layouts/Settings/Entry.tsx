/** @file Rendering for an arbitrary {@link SettingsEntryData}. */
import SettingsCustomEntry from './CustomEntry'
import { SettingsEntryType, type SettingsContext, type SettingsEntryData } from './data'
import SettingsInputEntry from './InputEntry'

// =====================
// === SettingsEntry ===
// =====================

/** Props for a {@link SettingsEntry}. */
export interface SettingsEntryProps {
  readonly context: SettingsContext
  readonly data: SettingsEntryData
}

/** Rendering for an arbitrary {@link SettingsEntryData}. */
export default function SettingsEntry(props: SettingsEntryProps) {
  const { context, data } = props
  switch (data.type) {
    case SettingsEntryType.input: {
      return <SettingsInputEntry context={context} data={data} />
    }
    case SettingsEntryType.custom: {
      return <SettingsCustomEntry context={context} data={data} />
    }
  }
}
