/** @file Rendering for an arbitrary {@link SettingsEntryData}. */
import { SettingsCustomEntry } from './CustomEntry'
import { type SettingsContext, type SettingsEntryData } from './data'
import { SettingsFormEntry } from './FormEntry'

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
    case 'form': {
      return <SettingsFormEntry context={context} data={data} />
    }
    case 'custom': {
      return <SettingsCustomEntry context={context} data={data} />
    }
  }
}
