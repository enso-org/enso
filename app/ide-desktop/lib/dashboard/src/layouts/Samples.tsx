/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import Logo from 'enso-assets/enso_logo.svg'
import GeoImage from 'enso-assets/geo.svg'
import HeartIcon from 'enso-assets/heart.svg'
import OpenCountIcon from 'enso-assets/open_count.svg'
import ProjectIcon from 'enso-assets/project_icon.svg'
import SpreadsheetsImage from 'enso-assets/spreadsheets.svg'
import VisualizeImage from 'enso-assets/visualize.png'

import Spinner, * as spinner from '#/components/Spinner'
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

// =====================
// === ProjectsEntry ===
// =====================

/** Props for an {@link ProjectsEntry}. */
interface InternalProjectsEntryProps {
  readonly createProject: (
    templateId: null,
    templateName: null,
    onSpinnerStateChange: ((state: spinner.SpinnerState | null) => void) | null
  ) => void
}

/** A button that, when clicked, creates and opens a new blank project. */
function ProjectsEntry(props: InternalProjectsEntryProps) {
  const { createProject } = props
  const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)

  const onClick = () => {
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
      <button
        // This UI element does not appear anywhere else.
        // eslint-disable-next-line no-restricted-syntax
        className="relative h-sample cursor-pointer before:absolute before:inset before:bg-frame before:rounded-default before:w-full before:h-full before:opacity-60"
        onClick={onClick}
      >
        <div className="relative flex rounded-default size-full">
          <div className="flex flex-col text-center items-center gap-new-empty-project m-auto">
            {spinnerState != null ? (
              <Spinner size={SPINNER_SIZE_PX} padding={2} state={spinnerState} />
            ) : (
              <img src={ProjectIcon} />
            )}
            <p className="font-semibold text-sm">New empty project</p>
          </div>
        </div>
      </button>
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

  const onClick = () => {
    setSpinnerState(spinner.SpinnerState.initial)
    createProject(id, title, onSpinnerStateChange)
  }

  return (
    <div className="flex flex-col gap-sample">
      <button
        key={title}
        className="relative flex flex-col grow cursor-pointer text-left h-sample"
        onClick={onClick}
      >
        <div
          style={{ background }}
          className={`rounded-t-default w-full h-sample-image ${
            background != null ? '' : 'bg-frame'
          }`}
        />
        <div className="grow bg-frame backdrop-blur rounded-b-default w-full px-sample-description-x pt-sample-description-t pb-sample-description-b">
          <h2 className="text-header text-sm font-bold">{title}</h2>
          <div className="text-xs text-ellipsis leading-snug">{description}</div>
        </div>
        {spinnerState != null && (
          <div className="absolute grid w-full h-sample-image place-items-center">
            <Spinner size={SPINNER_SIZE_PX} state={spinnerState} />
          </div>
        )}
      </button>
      {/* Although this component is instantiated multiple times, it has a unique role and hence
       * its own opacity. */}
      {/* eslint-disable-next-line no-restricted-syntax */}
      <div className="flex justify-between text-primary h-sample-info px-sample-description-x opacity-70">
        <div className="flex gap-samples-icon-with-text">
          <SvgMask src={Logo} className="size-icon self-end" />
          <span className="font-bold leading-snug self-start">{author}</span>
        </div>
        {/* Normally `flex` */}
        <div className="gap-icons hidden">
          <div title="Views" className="flex gap-samples-icon-with-text">
            <SvgMask alt="Views" src={OpenCountIcon} className="size-icon self-end" />
            <span className="font-bold leading-snug self-start">{opens}</span>
          </div>
          <div title="Likes" className="flex gap-samples-icon-with-text">
            <SvgMask alt="Likes" src={HeartIcon} className="size-icon self-end" />
            <span className="font-bold leading-snug self-start">{likes}</span>
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
  return (
    <div data-testid="samples" className="flex flex-col gap-subheading px-home-section-x">
      <h2 className="text-subheading">Sample and community projects</h2>
      <div className="grid gap-samples grid-cols-fill-samples">
        <ProjectsEntry createProject={createProject} />
        {SAMPLES.map(sample => (
          <ProjectTile key={sample.id} sample={sample} createProject={createProject} />
        ))}
      </div>
    </div>
  )
}
