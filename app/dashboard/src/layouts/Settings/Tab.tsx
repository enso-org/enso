/** @file Rendering for a settings section. */
import { Suspense, useMemo } from 'react'

import { twMerge } from 'tailwind-merge'

import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Loader } from '#/components/Loader'
import { usePaywall } from '#/hooks/billing'
import { useFullUserSession } from '#/providers/AuthProvider'
import type { SettingsContext, SettingsSectionData, SettingsTabData } from './data'
import SettingsPaywall from './Paywall'
import SettingsSection from './Section'

// ===================
// === SettingsTab ===
// ===================

/** Props for a {@link SettingsTab}. */
export interface SettingsTabProps {
  readonly context: SettingsContext
  readonly data: SettingsTabData
  readonly onInteracted: () => void
}

/** Styled content of a settings tab. */
export default function SettingsTab(props: SettingsTabProps) {
  const { context, data, onInteracted } = props
  const { sections } = data
  const { user } = useFullUserSession()
  const { isFeatureUnderPaywall } = usePaywall({ plan: user.plan })
  const paywallFeature =
    data.feature != null && isFeatureUnderPaywall(data.feature) ? data.feature : null
  const [columns, classes] = useMemo<
    [readonly (readonly SettingsSectionData[])[], readonly string[]]
  >(() => {
    const resultColumns: SettingsSectionData[][] = []
    const resultClasses: string[] = []
    for (const section of sections) {
      const columnNumber = section.column ?? 1
      while (resultColumns.length < columnNumber) {
        resultColumns.push([])
      }
      resultColumns[columnNumber - 1]?.push(section)
      while (resultClasses.length < columnNumber) {
        resultClasses.push('')
      }
      if (section.columnClassName != null) {
        const oldClasses = resultClasses[columnNumber - 1]
        resultClasses[columnNumber - 1] =
          oldClasses == null ? section.columnClassName : `${oldClasses} ${section.columnClassName}`
      }
    }
    return [resultColumns, resultClasses]
  }, [sections])

  const contentProps = {
    onMouseDown: onInteracted,
    onPointerDown: onInteracted,
    onFocus: onInteracted,
  }

  if (paywallFeature) {
    return <SettingsPaywall feature={paywallFeature} />
  } else {
    const content =
      columns.length === 1 ?
        <div className="flex grow flex-col gap-settings-subsection" {...contentProps}>
          {sections.map((section) => (
            <SettingsSection key={section.nameId} context={context} data={section} />
          ))}
        </div>
      : <div
          className="flex min-h-full grow flex-col gap-settings-section lg:h-auto lg:flex-row"
          {...contentProps}
        >
          {columns.map((sectionsInColumn, i) => (
            <div
              key={i}
              className={twMerge(
                'flex h-fit flex-1 flex-col gap-settings-subsection pb-12',
                classes[i],
              )}
            >
              {sectionsInColumn.map((section) => (
                <SettingsSection key={section.nameId} context={context} data={section} />
              ))}
            </div>
          ))}
        </div>

    return (
      <ErrorBoundary>
        <Suspense fallback={<Loader size="medium" minHeight="h64" />}>{content}</Suspense>
      </ErrorBoundary>
    )
  }
}
