/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import ReadAndFilterImage from '#/assets/ReadAndFilter.png'
import CleansingImage from '#/assets/cleansing.png'
import AggregatingImage from '#/assets/aggregate.png'
import JoinImage from '#/assets/joining.png'
import SpreadsheetsImage from '#/assets/spreadsheets.svg'
import BlankImage from '#/assets/blankProject.png'
import VisualizeImage from '#/assets/visualize.png'
import MonthSalesImage from '#/assets/monthSales.png'
import CovidImage from '#/assets/covid.png'
import WeatherImage from '#/assets/weather.png'
import GeoImage from '#/assets/geo.svg'
import NasdaqImage from '#/assets/nasdaq.png'
import KMeansImage from '#/assets/kmeans.png'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import * as tailwindMerge from '#/utilities/tailwindMerge'

// =========================
// === List of templates ===
// =========================

/** Template metadata. */
export interface Sample {
  readonly title: string
  /** These should ideally be localized, however, as this is planned to be user-generated, it is
   * unlikely that this will be feasible. */
  readonly description: string
  readonly id: string
  readonly background?: string
  readonly group?: string
}

/** The full list of templates. */
export const SAMPLES: Sample[] = [
  {
    title: 'Blank Project',
    id: 'Default',
    description: "Start with a blank workflow.",
    background: `url("${BlankImage}") center / cover`,
    group: 'Getting Started'
  },
  {
    title: 'Reading and Filterng Data',
    id: 'GettingStartedReading',
    description: "Learn how to bring data into Enso.",
    background: `url("${ReadAndFilterImage}") center / cover`,
    group: 'Getting Started'
  },
  {
    title: 'Aggregating and Pivoting',
    id: 'GettingStartedAggregating',
    description: "Learn how to group and aggregate data, and pivot.",
    background: `url("${AggregatingImage}") center / cover`,
    group: 'Getting Started'
  },
  {
    title: 'Cleaning and Parsing Data',
    id: 'GettingStartedCleansing',
    description: "Learn how to cleanse and parse text values.",
    background: `url("${CleansingImage}") center / cover`,
    group: 'Getting Started'
  },
  {
    title: 'Selecting Columns and Joining Tables',
    id: 'GettingStartedSelecting',
    description: "Learn how to choose columns and join tables.",
    background: `url("${JoinImage}") center / cover`,
    group: 'Getting Started'
  },
  {
    title: 'Analyze GitHub stars',
    id: 'Stargazers',
    description: "Find out which of Enso's repositories are most popular over time.",
    background: `url("${VisualizeImage}") center / cover`,
    group: 'Advanced'
  },
  {
    title: 'NASDAQ Returns',
    id: 'NASDAQReturns',
    description: 'Learn how to clean your data to prepare it for advanced analysis.',
    background: `url("${NasdaqImage}") center / cover`,
    group: 'Advanced'
  },
  {
    title: 'KMeans',
    id: 'KMeans',
    description: 'Learn where to open a coffee shop to maximize your income.',
    background: `url("${KMeansImage}") center / cover`,
    group: 'Advanced'
  },
  {
    title: 'Combine spreadsheets',
    id: 'Orders',
    description: 'Glue multiple spreadsheets together to analyse all your data at once.',
    background: `url('${SpreadsheetsImage}') center / 50% no-repeat, rgba(255, 255, 255, 0.30)`,
    group: 'Examples'
  },
  {
    title: 'Month on Month Sales',
    id: 'MonthlySales',
    description: 'Learn how to compare with previous month sales.',
    background: `url("${MonthSalesImage}") center / cover`,
    group: 'Examples'
  },
  {
    title: 'Colorado COVID',
    id: 'Colorado_COVID',
    description: 'Learn to glue multiple spreadsheets to analyses all your data at once.',
    background: `url('${CovidImage}') center / 100% no-repeat, rgba(255, 255, 255, 0.30)`,
    group: 'Examples'
  },
  {
    title: 'Web API analysis',
    id: 'BankHolidayRain',
    description: 'Learn whether it rains on UK Bank Holidays via REST APIs.',
    background: `url('${WeatherImage}') center / 100% no-repeat, rgba(255, 255, 255, 0.30)`,
    group: 'Examples'
  },
  {
    title: 'Geospatial analysis',
    id: 'Restaurants',
    description: 'Learn where to open a coffee shop to maximize your income.',
    background: `url('${GeoImage}') 50% 20% / 100% no-repeat`,
    group: 'Examples'
  },
]

// ===================
// === ProjectTile ===
// ===================

/** Props for a {@link ProjectTile}. */
interface InternalProjectTileProps {
  readonly sample: Sample
  readonly createProject: (templateId: string, templateName: string) => void
}

/** A button that, when clicked, creates and opens a new project based on a template. */
function ProjectTile(props: InternalProjectTileProps) {
  const { sample, createProject } = props
  const { id, title, description, background } = sample

  return (
    <div className="flex flex-col gap-sample">
      <FocusArea direction="horizontal">
        {(innerProps) => (
          <FocusRing placement="after">
            <aria.Button
              key={title}
              className="focus-child relative flex h-sample grow cursor-pointer flex-col text-left after:pointer-events-none after:absolute after:inset after:rounded-default"
              onPress={() => {
                createProject(id, title)
              }}
              {...innerProps}
            >
              <div
                style={{ background }}
                className={tailwindMerge.twMerge(
                  'h-sample-image w-full rounded-t-default',
                  background == null && 'bg-frame',
                )}
              />
              <div className="w-full grow rounded-b-default bg-frame px-sample-description-x pb-sample-description-b pt-sample-description-t backdrop-blur">
                <aria.Heading className="text-header text-sm font-bold">{title}</aria.Heading>
                <div className="text-ellipsis text-xs leading-snug">{description}</div>
              </div>
            </aria.Button>
          </FocusRing>
        )}
      </FocusArea>
      {/* Although this component is instantiated multiple times, it has a unique role and hence
       * its own opacity. */}
      {/* eslint-disable-next-line no-restricted-syntax */}
    </div>
  )
}

// ===============
// === Samples ===
// ===============

/** Props for a {@link Samples}. */
export interface SamplesProps {
  readonly groupName: string
  readonly createProject: (templateId?: string | null, templateName?: string | null) => void
}

/** A list of sample projects. */
export default function Samples(props: SamplesProps) {
  const { groupName, createProject } = props

  return (
    <div data-testid="samples" className="flex flex-col gap-subheading px-[5px]">
      <aria.Heading level={2} className="text-subheading font-normal">
        {groupName}
      </aria.Heading>

      <div className="grid grid-cols-fill-samples gap-samples">
        {SAMPLES.filter(s => s.group === groupName).map((sample) => (
          <ProjectTile key={sample.id} sample={sample} createProject={createProject} />
        ))}
      </div>
    </div>
  )
}
