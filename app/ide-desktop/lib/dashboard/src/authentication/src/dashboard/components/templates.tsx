/** @file Renders the list of templates from which a project can be created. */
import * as svg from '../../components/svg'

// =================
// === Templates ===
// =================

/** Template metadata. */
interface Template {
    title: string
    description: string
    id: string
    background: string
}

/** The full list of templates. */
const TEMPLATES: Template[] = [
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

/** Render all templates, and a button to create an empty project. */
interface TemplatesRenderProps {
    // Later this data may be requested and therefore needs to be passed dynamically.
    templates: Template[]
    onTemplateClick: (name: string | null) => void
}

function TemplatesRender(props: TemplatesRenderProps) {
    const { templates, onTemplateClick } = props

    /** The action button for creating an empty project. */
    const CreateEmptyTemplate = (
        <button
            onClick={() => {
                onTemplateClick(null)
            }}
            className="h-40 cursor-pointer"
        >
            <div className="flex h-full w-full border-dashed-custom rounded-2xl text-primary">
                <div className="m-auto text-center">
                    <button>{svg.CIRCLED_PLUS_ICON}</button>
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
                    className="h-40 cursor-pointer"
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

/** The `TemplatesRender`'s container. */
interface TemplatesProps {
    onTemplateClick: (name?: string | null) => void
}

function Templates(props: TemplatesProps) {
    const { onTemplateClick } = props

    return (
        <div className="bg-white my-2">
            <div className="mx-auto py-2 px-4 sm:py-4 sm:px-6 lg:px-8">
                <div className="grid gap-2 sm:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5">
                    <TemplatesRender templates={TEMPLATES} onTemplateClick={onTemplateClick} />
                </div>
            </div>
        </div>
    )
}
export default Templates
