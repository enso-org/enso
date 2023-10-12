/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import Logo from 'enso-assets/enso_logo.svg'
import GeoImage from 'enso-assets/geo.svg'
import HeartIcon from 'enso-assets/heart.svg'
import OpenCountIcon from 'enso-assets/open_count.svg'
import ProjectIcon from 'enso-assets/project_icon.svg'
import SpreadsheetsImage from 'enso-assets/spreadsheets.svg'
import VisualizeImage from 'enso-assets/visualize.png'

import SvgMask from '../../authentication/components/svgMask'
import Spinner, * as spinner from './spinner'

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
    title: string
    description: string
    id: string
    background?: string
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
    onTemplateClick: (
        name: null,
        onSpinnerStateChange: (spinnerState: spinner.SpinnerState | null) => void
    ) => void
}

/** A button that, when clicked, creates and opens a new blank project. */
function ProjectsEntry(props: InternalProjectsEntryProps) {
    const { onTemplateClick } = props
    const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)

    return (
        <div className="flex h-51 flex-col gap-1.5">
            <button
                className="relative grow cursor-pointer"
                onClick={() => {
                    setSpinnerState(spinner.SpinnerState.initial)
                    onTemplateClick(null, newSpinnerState => {
                        setSpinnerState(newSpinnerState)
                        if (newSpinnerState === spinner.SpinnerState.done) {
                            window.setTimeout(() => {
                                setSpinnerState(null)
                            }, SPINNER_DONE_DURATION_MS)
                        }
                    })
                }}
            >
                <div className="absolute h-full w-full rounded-2xl bg-frame opacity-60" />
                <div className="relative flex h-full w-full rounded-2xl">
                    <div className="m-auto flex flex-col items-center gap-3 text-center">
                        {spinnerState != null ? (
                            <div className="p-2">
                                <Spinner size={SPINNER_SIZE_PX} state={spinnerState} />
                            </div>
                        ) : (
                            <img src={ProjectIcon} />
                        )}
                        <p className="text-sm font-semibold">New empty project</p>
                    </div>
                </div>
            </button>
            <div className="h-4.5" />
        </div>
    )
}

// ===================
// === ProjectTile ===
// ===================

/** Props for a {@link ProjectTile}. */
interface InternalProjectTileProps {
    template: Sample
    onTemplateClick: (
        name: string | null,
        onSpinnerStateChange: (spinnerState: spinner.SpinnerState | null) => void
    ) => void
}

/** A button that, when clicked, creates and opens a new project based on a template. */
function ProjectTile(props: InternalProjectTileProps) {
    const { template, onTemplateClick } = props
    const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)
    const author = DUMMY_AUTHOR
    const opens = DUMMY_OPEN_COUNT
    const likes = DUMMY_LIKE_COUNT

    const onSpinnerStateChange = React.useCallback(
        (newSpinnerState: spinner.SpinnerState | null) => {
            setSpinnerState(newSpinnerState)
            if (newSpinnerState === spinner.SpinnerState.done) {
                window.setTimeout(() => {
                    setSpinnerState(null)
                }, SPINNER_DONE_DURATION_MS)
            }
        },
        []
    )

    return (
        <div className="flex h-51 flex-col gap-1.5">
            <button
                key={template.title}
                className="relative flex grow cursor-pointer flex-col text-left"
                onClick={() => {
                    setSpinnerState(spinner.SpinnerState.initial)
                    onTemplateClick(template.id, onSpinnerStateChange)
                }}
            >
                <div
                    style={{
                        background: template.background,
                    }}
                    className={`h-25 w-full rounded-t-2xl ${
                        template.background != null ? '' : 'bg-frame'
                    }`}
                />
                <div className="w-full grow rounded-b-2xl bg-frame px-4 pb-3.5 pt-1.75 backdrop-blur">
                    <h2 className="py-0.5 text-sm font-bold leading-144.5">{template.title}</h2>
                    <div className="text-ellipsis pb-px text-xs leading-144.5">
                        {template.description}
                    </div>
                </div>
                {spinnerState != null && (
                    <div className="absolute grid h-25 w-full place-items-center">
                        <Spinner size={SPINNER_SIZE_PX} state={spinnerState} />
                    </div>
                )}
            </button>
            <div className="flex h-4.5 justify-between px-4 text-primary opacity-70">
                <div className="flex gap-1.5">
                    <SvgMask src={Logo} />
                    <span className="pb-px font-bold leading-144.5">{author}</span>
                </div>
                <div className="flex gap-3">
                    {/* Opens */}
                    <div className="flex gap-1.5">
                        <SvgMask src={OpenCountIcon} />
                        <span className="pb-px font-bold leading-144.5">{opens}</span>
                    </div>
                    {/* Likes */}
                    <div className="flex gap-1.5">
                        <SvgMask src={HeartIcon} />
                        <span className="pb-px font-bold leading-144.5">{likes}</span>
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
    onTemplateClick: (
        name: string | null,
        onSpinnerStateChange: (state: spinner.SpinnerState | null) => void
    ) => void
}

/** A list of sample projects. */
export default function Samples(props: SamplesProps) {
    const { onTemplateClick } = props
    return (
        <div className="flex flex-col gap-4 px-4.75">
            <h2 className="py-0.5 text-xl leading-144.5">Sample and community projects</h2>
            <div className="grid grid-cols-fill-60 gap-2">
                <ProjectsEntry onTemplateClick={onTemplateClick} />
                {SAMPLES.map(template => (
                    <ProjectTile
                        key={template.id}
                        template={template}
                        onTemplateClick={onTemplateClick}
                    />
                ))}
            </div>
        </div>
    )
}
