/** @file Rendering for a settings section. */
import { Text } from '#/components/AriaComponents'
import FocusArea from '#/components/styled/FocusArea'
import { useText } from '#/providers/TextProvider'
import type { SettingsContext, SettingsSectionData } from './data'
import SettingsEntry from './Entry'

// =======================
// === SettingsSection ===
// =======================

/** Props for a {@link SettingsSection}. */
export interface SettingsSectionProps {
  readonly context: SettingsContext
  readonly data: SettingsSectionData
}

/** Rendering for a settings section. */
export default function SettingsSection(props: SettingsSectionProps) {
  const { context, data } = props
  const { nameId, focusArea = true, heading = true, entries } = data
  const { getText } = useText()
  const isVisible = entries.some((entry) => entry.getVisible?.(context) ?? true)

  return !isVisible ? null : (
      <FocusArea active={focusArea} direction="vertical">
        {(innerProps) => (
          <div className="flex w-full flex-col gap-settings-section-header" {...innerProps}>
            {!heading ? null : (
              <Text.Heading level={2} weight="bold">
                {getText(nameId)}
              </Text.Heading>
            )}
            <div className="flex flex-col">
              {entries.map((entry, i) => (
                <SettingsEntry key={i} context={context} data={entry} />
              ))}
            </div>
          </div>
        )}
      </FocusArea>
    )
}
