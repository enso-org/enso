/** @file Renders the list of templates from which a project can be created. */
import * as react from 'react'

import PlusCircledIcon from 'enso-assets/plus_circled.svg'
import PlusIcon from 'enso-assets/plus.svg'
import RotatingArrowIcon from 'enso-assets/rotating_arrow.svg'

import * as common from 'enso-common'

// =================
// === Constants ===
// =================

/** The `localStorage` key used to store whether the {@link Templates} element should be
 * expanded by default. */
const IS_TEMPLATES_OPEN_KEY = `${common.PRODUCT_NAME.toLowerCase()}-is-templates-expanded`

// =============
// === Types ===
// =============

/** The CSS class to apply inset shadows on the specified side(s). */
enum ShadowClass {
    none = '',
    top = 'shadow-inset-t-lg',
    bottom = 'shadow-inset-b-lg',
    both = 'shadow-inset-v-lg',
}

// =================
// === Templates ===
// =================

/** Template metadata. */
export interface Template {
    title: string
    description: string
    id: string
    background: string
}

/** The full list of templates. */
export const TEMPLATES: [Template, ...Template[]] = [
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
        background: 'url("/spreadsheets.png") 50% 20% / 80% no-repeat, #479366',
    },
    {
        title: 'Geospatial analysis',
        id: 'Restaurants',
        description: 'Learn where to open a coffee shop to maximize your income.',
        background: 'url("/geo.png") center / cover',
    },
    {
        title: 'Analyze GitHub stars',
        id: 'Stargazers',
        description: "Find out which of Enso's repositories are most popular over time.",
        background: 'url("/visualize.png") center / cover',
    },
]

// =======================
// === TemplatesRender ===
// =======================

/** Props for a {@link TemplatesRender}. */
export interface TemplatesRenderProps {
    // Later this data may be requested and therefore needs to be passed dynamically.
    templates: Template[]
    onTemplateClick: (name: string | null) => void
}

/** Render all templates, and a button to create an empty project. */
function TemplatesRender(props: TemplatesRenderProps) {
    const { templates, onTemplateClick } = props

    /** The action button for creating an empty project. */
    const CreateEmptyTemplate = (
        <button
            onClick={() => {
                onTemplateClick(null)
            }}
            className="h-40 w-60 cursor-pointer"
        >
            <div className="flex h-full w-full border-dashed-custom rounded-2xl text-primary">
                <div className="m-auto text-center">
                    <button>
                        <img src={PlusCircledIcon} />
                    </button>
                    <p className="font-semibold text-sm">New empty project</p>
                </div>
            </div>
        </button>
    )

    return (
        <>
            {CreateEmptyTemplate}
            {templates.map(template => (
                <button
                    key={template.title}
                    className="h-40 w-60 cursor-pointer"
                    onClick={() => {
                        onTemplateClick(template.id)
                    }}
                >
                    <div
                        style={{
                            background: template.background,
                        }}
                        className="flex flex-col justify-end h-full w-full rounded-2xl overflow-hidden text-white text-left"
                    >
                        <div className="bg-black bg-opacity-30 px-4 py-2">
                            <h2 className="text-sm font-bold">{template.title}</h2>
                            <div className="text-xs h-16 text-ellipsis py-2">
                                {template.description}
                            </div>
                        </div>
                    </div>
                </button>
            ))}
        </>
    )
}

// =================
// === Templates ===
// =================

/** Props for a {@link Templates}. */
export interface TemplatesProps {
    onTemplateClick: (name?: string | null) => void
}

/** A container for a {@link TemplatesRender} which passes it a list of templates. */
function Templates(props: TemplatesProps) {
    const { onTemplateClick } = props
    // This is incorrect, but SAFE, as its value will always be assigned before any hooks are run.
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const containerRef = react.useRef<HTMLDivElement>(null!)
    const [shadowClass, setShadowClass] = react.useState(ShadowClass.none)
    const [isOpen, setIsOpen] = react.useState(() => {
        /** This must not be in a `useEffect` as it would flash open for one frame.
         * It can be in a `useLayoutEffect` but as that needs to be checked every re-render,
         * this is slightly more performant. */
        const savedIsOpen = localStorage.getItem(IS_TEMPLATES_OPEN_KEY)
        let result = true
        if (savedIsOpen != null) {
            try {
                result = JSON.parse(savedIsOpen) !== false
            } catch {
                // Ignored. This should only happen when a user manually sets invalid JSON into
                // the `localStorage` key used by this component.
            }
        }
        return result
    })

    const toggleIsOpen = () => {
        setIsOpen(!isOpen)
    }

    const updateShadowClass = () => {
        const element = containerRef.current
        const boundingBox = element.getBoundingClientRect()
        let newShadowClass: ShadowClass
        const shouldShowTopShadow = element.scrollTop !== 0
        const shouldShowBottomShadow = element.scrollTop + boundingBox.height < element.scrollHeight
        if (shouldShowTopShadow && shouldShowBottomShadow) {
            newShadowClass = ShadowClass.both
        } else if (shouldShowTopShadow) {
            newShadowClass = ShadowClass.top
        } else if (shouldShowBottomShadow) {
            newShadowClass = ShadowClass.bottom
        } else {
            newShadowClass = ShadowClass.none
        }
        setShadowClass(newShadowClass)
    }

    react.useEffect(() => {
        window.addEventListener('resize', updateShadowClass)
        return () => {
            window.removeEventListener('resize', updateShadowClass)
        }
    })

    react.useEffect(() => {
        updateShadowClass()
    }, [])

    react.useEffect(() => {
        localStorage.setItem(IS_TEMPLATES_OPEN_KEY, JSON.stringify(isOpen))
    }, [isOpen])

    return (
        <div className="mx-2">
            <div className="flex items-center my-2">
                <div className="w-4">
                    <div
                        className={`cursor-pointer transition-all ease-in-out ${
                            isOpen ? 'rotate-90' : ''
                        }`}
                        onClick={toggleIsOpen}
                    >
                        <img src={RotatingArrowIcon} />
                    </div>
                </div>
                <h1 className="text-xl font-bold self-center">Templates</h1>
                {!isOpen && (
                    <button
                        className="bg-gray-100 rounded-full p-1 mx-2"
                        onClick={() => {
                            onTemplateClick()
                        }}
                    >
                        <img src={PlusIcon} className="inline" /> New blank project&nbsp;
                    </button>
                )}
            </div>
            <div
                ref={containerRef}
                className={`grid gap-2 grid-cols-fill-60 justify-center overflow-y-scroll scroll-hidden transition-all duration-300 ease-in-out ${
                    isOpen ? `h-80 ${shadowClass}` : 'h-0'
                }`}
                onScroll={updateShadowClass}
            >
                <TemplatesRender templates={TEMPLATES} onTemplateClick={onTemplateClick} />
            </div>
        </div>
    )
}
export default Templates
