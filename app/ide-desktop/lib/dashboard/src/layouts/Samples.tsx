/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import Logo from 'enso-assets/enso_logo.svg'
import GeoImage from 'enso-assets/geo.svg'
import HeartIcon from 'enso-assets/heart.svg'
import OpenCountIcon from 'enso-assets/open_count.svg'
import ProjectIcon from 'enso-assets/project_icon.svg'
import SpreadsheetsImage from 'enso-assets/spreadsheets.svg'
import VisualizeImage from 'enso-assets/visualize.png'

import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import Spinner, * as spinner from '#/components/Spinner'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import SvgMask from '#/components/SvgMask'

// =================
// === Constants ===
// =================

/** The size (both width and height) of the spinner, in pixels. */
const SPINNER_SIZE_PX = 50
/** The duration of the "spinner done" animation. */
const SPINNER_DONE_DURATION_MS = 1000
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

// ========================
// === BlankProjectTile ===
// ========================

/** Props for an {@link BlankProjectTile}. */
interface InternalBlankProjectTileProps {
  readonly createProject: (
    templateId: null,
    templateName: null,
    onSpinnerStateChange: ((state: spinner.SpinnerState | null) => void) | null
  ) => void
}

/** A button that, when clicked, creates and opens a new blank project. */
function BlankProjectTile(props: InternalBlankProjectTileProps) {
  const { createProject } = props
  const { getText } = textProvider.useText()
  const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)

  const onPress = () => {
    setSpinnerState(spinner.SpinnerState.initial)
    createProject(null, null, newSpinnerState => {
      setSpinnerState(newSpinnerState)
      if (newSpinnerState === spinner.SpinnerState.done) {
        window.setTimeout(() => {
          setSpinnerState(null)
        }, SPINNER_DONE_DURATION_MS)
      }
    })
  }

  return (
    <div className="flex flex-col gap-sample">
      <FocusArea direction="horizontal">
        {innerProps => (
          <FocusRing placement="after">
            <aria.Button
              className="focus-child relative h-sample cursor-pointer before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-frame before:opacity-60 after:pointer-events-none after:absolute after:inset after:rounded-default"
              onPress={onPress}
              {...innerProps}
            >
              <div className="relative flex size-full rounded-default">
                <div className="m-auto flex flex-col items-center gap-new-empty-project text-center">
                  {spinnerState != null ? (
                    <Spinner size={SPINNER_SIZE_PX} padding={2} state={spinnerState} />
                  ) : (
                    <img src={ProjectIcon} />
                  )}
                  <p className="text-sm font-semibold">{getText('newEmptyProject')}</p>
                </div>
              </div>
            </aria.Button>
          </FocusRing>
        )}
      </FocusArea>
      <div className="h-sample-info" />
    </div>
  )
}

// ===================
// === ProjectTile ===
// ===================

/** Props for a {@link ProjectTile}. */
interface InternalProjectTileProps {
  readonly sample: Sample
  readonly createProject: (
    templateId: string,
    templateName: string,
    onSpinnerStateChange: (state: spinner.SpinnerState | null) => void
  ) => void
}

/** A button that, when clicked, creates and opens a new project based on a template. */
function ProjectTile(props: InternalProjectTileProps) {
  const { sample, createProject } = props
  const { getText } = textProvider.useText()
  const { id, title, description, background } = sample
  const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)
  const author = DUMMY_AUTHOR
  const opens = DUMMY_OPEN_COUNT
  const likes = DUMMY_LIKE_COUNT

  const onSpinnerStateChange = React.useCallback((newSpinnerState: spinner.SpinnerState | null) => {
    setSpinnerState(newSpinnerState)
    if (newSpinnerState === spinner.SpinnerState.done) {
      window.setTimeout(() => {
        setSpinnerState(null)
      }, SPINNER_DONE_DURATION_MS)
    }
  }, [])

  const onPress = () => {
    setSpinnerState(spinner.SpinnerState.initial)
    createProject(id, title, onSpinnerStateChange)
  }

  return (
    <div className="flex flex-col gap-sample">
      <FocusArea direction="horizontal">
        {innerProps => (
          <FocusRing placement="after">
            <aria.Button
              key={title}
              className="focus-child relative flex h-sample grow cursor-pointer flex-col text-left after:pointer-events-none after:absolute after:inset after:rounded-default"
              onPress={onPress}
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
              {spinnerState != null && (
                <div className="absolute grid h-sample-image w-full place-items-center">
                  <Spinner size={SPINNER_SIZE_PX} state={spinnerState} />
                </div>
              )}
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
  readonly createProject: (
    templateId?: string | null,
    templateName?: string | null,
    onSpinnerStateChange?: ((state: spinner.SpinnerState | null) => void) | null
  ) => void
}

/** A list of sample projects. */
export default function Samples(props: SamplesProps) {
  const { createProject } = props
  const { getText } = textProvider.useText()

  return (
    <div data-testid="samples" className="flex flex-col gap-subheading px-home-section-x">
      <aria.Heading level={2} className="text-subheading">
        {getText('sampleAndCommunityProjects')}
      </aria.Heading>
      <div className="grid grid-cols-fill-samples gap-samples">
        <BlankProjectTile createProject={createProject} />
        {SAMPLES.map(sample => (
          <ProjectTile key={sample.id} sample={sample} createProject={createProject} />
        ))}
      </div>
    </div>
  )
}
