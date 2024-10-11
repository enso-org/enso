/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import ReadAndFilterImage from '#/assets/ReadAndFilter.png'
import AggregatingImage from '#/assets/aggregate.png'
import BlankImage from '#/assets/blankProject.png'
import CleansingImage from '#/assets/cleansing.png'
import CovidImage from '#/assets/covid.png'
import GeoImage from '#/assets/geo.svg'
import JoinImage from '#/assets/joining.png'
import KMeansImage from '#/assets/kmeans.png'
import MonthSalesImage from '#/assets/monthSales.png'
import NasdaqImage from '#/assets/nasdaq.png'
import SpreadsheetsImage from '#/assets/spreadsheets.svg'
import VisualizeImage from '#/assets/visualize.png'
import WeatherImage from '#/assets/weather.png'

import { Button, Text } from '#/components/AriaComponents'

// =========================
// === List of templates ===
// =========================

/** Template metadata. */
export interface Sample {
  readonly title: string
  /**
   * These should ideally be localized, however, as this is planned to be user-generated, it is
   * unlikely that this will be feasible.
   */
  readonly description: string
  readonly id: string
  readonly background?: string
  readonly group?: string
}

/** The full list of templates. */
export const SAMPLES: Sample[] = [
  {
    title: 'New Project',
    id: 'Default',
    description: '',
    background: `url("${BlankImage}") center / cover`,
    group: 'Get Started',
  },
  {
    title: 'Reading and Filtering Data',
    id: 'Getting_Started_Reading',
    description: 'Learn how to bring data into Enso.',
    background: `url("${ReadAndFilterImage}") center / cover`,
    group: 'Get Started',
  },
  {
    title: 'Aggregating and Pivoting',
    id: 'Getting_Started_Aggregating',
    description: 'Learn how to group and aggregate data, and pivot.',
    background: `url("${AggregatingImage}") center / cover`,
    group: 'Get Started',
  },
  {
    title: 'Cleaning and Parsing Data',
    id: 'Getting_Started_Cleansing',
    description: 'Learn how to cleanse and parse text values.',
    background: `url("${CleansingImage}") center / cover`,
    group: 'Get Started',
  },
  {
    title: 'Selecting Columns and Joining Tables',
    id: 'Getting_Started_Selecting',
    description: 'Learn how to choose columns and join tables.',
    background: `url("${JoinImage}") center / cover`,
    group: 'Get Started',
  },
  {
    title: 'Analyze GitHub stars',
    id: 'Stargazers',
    description: "Find out which of Enso's repositories are most popular over time.",
    background: `url("${VisualizeImage}") center / cover`,
    group: 'Advanced',
  },
  {
    title: 'NASDAQ Returns',
    id: 'NASDAQReturns',
    description: 'Learn how to clean your data to prepare it for advanced analysis.',
    background: `url("${NasdaqImage}") center / cover`,
    group: 'Advanced',
  },
  {
    title: 'KMeans',
    id: 'KMeans',
    description: 'Learn where to open a coffee shop to maximize your income.',
    background: `url("${KMeansImage}") center / cover`,
    group: 'Advanced',
  },
  {
    title: 'Combine spreadsheets',
    id: 'Orders',
    description: 'Glue multiple spreadsheets together to analyse all your data at once.',
    background: `url("${SpreadsheetsImage}") center / 50% no-repeat, rgba(255, 255, 255, 0.30)`,
    group: 'Examples',
  },
  {
    title: 'Month on Month Sales',
    id: 'Monthly_Sales',
    description: 'Learn how to compare with previous month sales.',
    background: `url("${MonthSalesImage}") center / cover`,
    group: 'Examples',
  },
  {
    title: 'Colorado COVID',
    id: 'Colorado_COVID',
    description: 'Learn to glue multiple spreadsheets to analyses all your data at once.',
    background: `url('${CovidImage}') center / 100% no-repeat, rgba(255, 255, 255, 0.30)`,
    group: 'Examples',
  },
  {
    title: 'Web API analysis',
    id: 'Bank_Holiday_Rain',
    description: 'Learn whether it rains on UK Bank Holidays via REST APIs.',
    background: `url('${WeatherImage}') center / 100% no-repeat, rgba(255, 255, 255, 0.30)`,
    group: 'Examples',
  },
  {
    title: 'Geospatial analysis',
    id: 'Restaurants',
    description: 'Learn where to open a coffee shop to maximize your income.',
    background: `url('${GeoImage}') 50% 20% / 100% no-repeat`,
    group: 'Examples',
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
    <Button
      variant="custom"
      size="custom"
      rounded="xxxlarge"
      key={title}
      className="flex-none snap-center snap-always overflow-hidden"
      style={{ background }}
      onPress={() => {
        createProject(id, title)
      }}
    >
      <div className="flex aspect-[7/4] h-40 w-full flex-col justify-end bg-gradient-to-t from-primary/80 to-transparent">
        <div className="flex w-full flex-col items-start px-4 pb-3 text-start">
          <Text variant="subtitle" color="invert" nowrap="normal">
            {title}
          </Text>
          <Text variant="body-sm" color="invert" nowrap="normal">
            {description}
          </Text>
        </div>
      </div>
    </Button>
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
    <div data-testid="samples" className="flex w-full flex-col">
      <Text.Heading level={2}>{groupName}</Text.Heading>

      <div className="-mx-12 inline-flex snap-x snap-mandatory gap-4 overflow-x-auto px-12 py-2 scroll-offset-edge-9xl">
        {SAMPLES.filter((s) => s.group === groupName).map((sample) => (
          <ProjectTile key={sample.id} sample={sample} createProject={createProject} />
        ))}
      </div>
    </div>
  )
}
