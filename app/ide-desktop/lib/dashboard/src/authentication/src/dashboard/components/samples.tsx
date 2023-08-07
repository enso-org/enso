/** @file Renders the list of templates from which a project can be created. */
import * as React from 'react'

import PlusCircledIcon from 'enso-assets/plus_circled.svg'

import GeoImage from 'enso-assets/geo.png'
import SpreadsheetsImage from 'enso-assets/spreadsheets.png'
import VisualizeImage from 'enso-assets/visualize.png'

import Spinner, * as spinner from './spinner'

// =================
// === Constants ===
// =================

/** The size (both width and height) of the spinner, in pixels. */
const SPINNER_SIZE = 64
/** The duration of the "spinner done" animation. */
const SPINNER_DONE_DURATION_MS = 1000

// =========================
// === List of templates ===
// =========================

/** Template metadata. */
export interface Sample {
    title: string
    description: string
    id: string
    background: string
}

/** The full list of templates. */
export const SAMPLES: Sample[] = [
    {
        title: 'Colorado COVID',
        id: 'Colorado_COVID',
        description: 'Learn to glue multiple spreadsheets to analyses all your data at once.',
        background: '#6b7280',
    },
    {
        title: 'KMeans',
        id: 'KMeans',
        description: 'Learn where to open a coffee shop to maximize your income.',
        background: '#6b7280',
    },
    {
        title: 'NASDAQ Returns',
        id: 'NASDAQReturns',
        description: 'Learn how to clean your data to prepare it for advanced analysis.',
        background: '#6b7280',
    },
    {
        title: 'Combine spreadsheets',
        id: 'Orders',
        description: 'Glue multiple spreadsheets together to analyse all your data at once.',
        background: `url("${SpreadsheetsImage}") 50% 11% / 50% no-repeat, #479366`,
    },
    {
        title: 'Geospatial analysis',
        id: 'Restaurants',
        description: 'Learn where to open a coffee shop to maximize your income.',
        background: `url("${GeoImage}") 50% 0% / 186.7768% no-repeat, #181818`,
    },
    {
        title: 'Analyze GitHub stars',
        id: 'Stargazers',
        description: "Find out which of Enso's repositories are most popular over time.",
        background: `url("${VisualizeImage}") center / cover, #dddddd`,
    },
]

// ==========================
// === EmptyProjectButton ===
// ==========================

/** Props for an {@link EmptyProjectButton}. */
interface InternalEmptyProjectButtonProps {
    onTemplateClick: (
        name: null,
        onSpinnerStateChange: (spinnerState: spinner.SpinnerState | null) => void
    ) => void
}

/** A button that, when clicked, creates and opens a new blank project. */
function EmptyProjectButton(props: InternalEmptyProjectButtonProps) {
    const { onTemplateClick } = props
    const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)

    return (
        <button
            onClick={() => {
                setSpinnerState(spinner.SpinnerState.initial)
                onTemplateClick(null, newSpinnerState => {
                    setSpinnerState(newSpinnerState)
                    if (newSpinnerState === spinner.SpinnerState.done) {
                        setTimeout(() => {
                            setSpinnerState(null)
                        }, SPINNER_DONE_DURATION_MS)
                    }
                })
            }}
            className="cursor-pointer relative text-primary h-40"
        >
            <div className="flex h-full w-full rounded-2xl">
                <div className="flex flex-col text-center items-center m-auto">
                    {spinnerState != null ? (
                        <div className="p-2">
                            <Spinner size={SPINNER_SIZE} state={spinnerState} />
                        </div>
                    ) : (
                        <img src={PlusCircledIcon} />
                    )}
                    <p className="font-semibold text-sm">New empty project</p>
                </div>
            </div>
        </button>
    )
}

// ======================
// === TemplateButton ===
// ======================

/** Props for a {@link TemplateButton}. */
interface InternalTemplateButtonProps {
    template: Sample
    onTemplateClick: (
        name: string | null,
        onSpinnerStateChange: (spinnerState: spinner.SpinnerState | null) => void
    ) => void
}

/** A button that, when clicked, creates and opens a new project based on a template. */
function TemplateButton(props: InternalTemplateButtonProps) {
    const { template, onTemplateClick } = props
    const [spinnerState, setSpinnerState] = React.useState<spinner.SpinnerState | null>(null)

    const onSpinnerStateChange = React.useCallback(
        (newSpinnerState: spinner.SpinnerState | null) => {
            setSpinnerState(newSpinnerState)
            if (newSpinnerState === spinner.SpinnerState.done) {
                setTimeout(() => {
                    setSpinnerState(null)
                }, SPINNER_DONE_DURATION_MS)
            }
        },
        []
    )

    return (
        <button
            key={template.title}
            className="h-40 cursor-pointer"
            onClick={() => {
                setSpinnerState(spinner.SpinnerState.initial)
                onTemplateClick(template.id, onSpinnerStateChange)
            }}
        >
            <div
                style={{
                    background: template.background,
                }}
                className="relative flex flex-col justify-end h-full w-full rounded-2xl overflow-hidden text-white text-left"
            >
                <div className="bg-black bg-opacity-30 px-4 py-2">
                    <h2 className="text-sm font-bold">{template.title}</h2>
                    <div className="text-xs h-16 text-ellipsis py-2">{template.description}</div>
                </div>
                {spinnerState != null && (
                    <div className="absolute grid w-full h-full place-items-center">
                        <Spinner size={SPINNER_SIZE} state={spinnerState} />
                    </div>
                )}
            </div>
        </button>
    )
}

// ===============
// === Samples ===
// ===============

/** Props for a {@link Samples}. */
export interface SamplesProps {
    visible: boolean
    onTemplateClick: (
        name: string | null,
        onSpinnerStateChange: (state: spinner.SpinnerState | null) => void
    ) => void
}

/** A list of sample projects. */
export default function Samples(props: SamplesProps) {
    const { visible, onTemplateClick } = props
    return (
        <div className={`flex flex-col gap-4 px-4.75 ${visible ? '' : 'hidden'}`}>
            <h2 className="text-xl leading-144.5 py-0.5">Sample and community projects</h2>
            <div className="grid gap-2 grid-cols-fill-60">
                <EmptyProjectButton onTemplateClick={onTemplateClick} />
                {SAMPLES.map(template => (
                    <TemplateButton
                        key={template.id}
                        template={template}
                        onTemplateClick={onTemplateClick}
                    />
                ))}
            </div>
        </div>
    )
}
