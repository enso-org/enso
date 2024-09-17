/** @file Rendering for a settings section. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as billing from '#/hooks/billing'

import * as authProvider from '#/providers/AuthProvider'

import type * as settingsData from '#/layouts/Settings/settingsData'
import SettingsPaywall from '#/layouts/Settings/SettingsPaywall'
import SettingsSection from '#/layouts/Settings/SettingsSection'

import * as errorBoundary from '#/components/ErrorBoundary'
import * as loader from '#/components/Loader'

// ===================
// === SettingsTab ===
// ===================

/** Props for a {@link SettingsTab}. */
export interface SettingsTabProps {
  readonly context: settingsData.SettingsContext
  readonly data: settingsData.SettingsTabData
  readonly onInteracted: () => void
}

/** Styled content of a settings tab. */
export default function SettingsTab(props: SettingsTabProps) {
  const { context, data, onInteracted } = props
  const { sections } = data
  const { user } = authProvider.useFullUserSession()
  const { isFeatureUnderPaywall } = billing.usePaywall({ plan: user.plan })
  const paywallFeature =
    data.feature != null && isFeatureUnderPaywall(data.feature) ? data.feature : null
  const [columns, classes] = React.useMemo<
    [readonly (readonly settingsData.SettingsSectionData[])[], readonly string[]]
  >(() => {
    const resultColumns: settingsData.SettingsSectionData[][] = []
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
              className={tailwindMerge.twMerge(
                'flex h-fit w-0 flex-1 flex-col gap-settings-subsection pb-12',
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
      <errorBoundary.ErrorBoundary>
        <React.Suspense fallback={<loader.Loader size="medium" minHeight="h64" />}>
          {content}
        </React.Suspense>
      </errorBoundary.ErrorBoundary>
    )
  }
}
