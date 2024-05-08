/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import Logo from 'enso-assets/enso_logo.svg'
import GeoImage from 'enso-assets/geo.svg'
import HeartIcon from 'enso-assets/heart.svg'
import OpenCountIcon from 'enso-assets/open_count.svg'
import SpreadsheetsImage from 'enso-assets/spreadsheets.svg'
import VisualizeImage from 'enso-assets/visualize.png'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

// =================
// === Constants ===
// =================

/** A placeholder author for a sample, for use until the backend implements an endpoint. */
const DUMMY_AUTHOR = 'Enso Team'
/** A placeholder number of times a sample has been opened, for use until the backend implements
 * an endpoint. */
const DUMMY_OPEN_COUNT = 10
/** A placeholder number of likes for a sample, for use until the backend implements an endpoint. */
const DUMMY_LIKE_COUNT = 10

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
}

/** The full list of templates. */
export const SAMPLES: Sample[] = [
  {
    title: 'Colorado COVID',
    id: 'Colorado_COVID',
    description: 'Learn to glue multiple spreadsheets to analyses all your data at once.',
  },
  {
    title: 'KMeans',
    id: 'KMeans',
    description: 'Learn where to open a coffee shop to maximize your income.',
  },
  {
    title: 'NASDAQ Returns',
    id: 'NASDAQReturns',
    description: 'Learn how to clean your data to prepare it for advanced analysis.',
  },
  {
    title: 'Combine spreadsheets',
    id: 'Orders',
    description: 'Glue multiple spreadsheets together to analyse all your data at once.',
    background: `url('${SpreadsheetsImage}') center / 50% no-repeat, rgba(255, 255, 255, 0.30)`,
  },
  {
    title: 'Geospatial analysis',
    id: 'Restaurants',
    description: 'Learn where to open a coffee shop to maximize your income.',
    background: `url('${GeoImage}') 50% 20% / 100% no-repeat`,
  },
  {
    title: 'Analyze GitHub stars',
    id: 'Stargazers',
    description: "Find out which of Enso's repositories are most popular over time.",
    background: `url("${VisualizeImage}") center / cover`,
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
  const { getText } = textProvider.useText()
  const { id, title, description, background } = sample
  const author = DUMMY_AUTHOR
  const opens = DUMMY_OPEN_COUNT
  const likes = DUMMY_LIKE_COUNT

  return (
    <div className="flex flex-col gap-sample">
      <FocusArea direction="horizontal">
        {innerProps => (
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
                className={`h-sample-image w-full rounded-t-default ${
                  background != null ? '' : 'bg-frame'
                }`}
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
      <div className="flex h-sample-info justify-between px-sample-description-x text-primary opacity-70">
        <div className="flex gap-samples-icon-with-text">
          <SvgMask src={Logo} className="size-icon self-end" />
          <aria.Text className="self-start font-bold leading-snug">{author}</aria.Text>
        </div>
        {/* Normally `flex` */}
        <div className="hidden gap-icons">
          <div title={getText('views')} className="flex gap-samples-icon-with-text">
            <SvgMask alt={getText('views')} src={OpenCountIcon} className="size-icon self-end" />
            <aria.Text className="self-start font-bold leading-snug">{opens}</aria.Text>
          </div>
          <div title={getText('likes')} className="flex gap-samples-icon-with-text">
            <SvgMask alt={getText('likes')} src={HeartIcon} className="size-icon self-end" />
            <aria.Text className="self-start font-bold leading-snug">{likes}</aria.Text>
          </div>
        </div>
      </div>
    </div>
  )
}

// ===============
// === Samples ===
// ===============

/** Props for a {@link Samples}. */
export interface SamplesProps {
  readonly createProject: (templateId?: string | null, templateName?: string | null) => void
}

/** A list of sample projects. */
export default function Samples(props: SamplesProps) {
  const { createProject } = props
  const { getText } = textProvider.useText()

  return (
    <div data-testid="samples" className="flex flex-col gap-subheading px-home-section-x">
      <aria.Heading level={2} className="text-subheading font-normal">
        {getText('sampleAndCommunityProjects')}
      </aria.Heading>
      <div className="grid grid-cols-fill-samples gap-samples">
        {SAMPLES.map(sample => (
          <ProjectTile key={sample.id} sample={sample} createProject={createProject} />
        ))}
      </div>
    </div>
  )
}
