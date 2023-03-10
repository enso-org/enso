/** @file Renders the list of templates that can be used to create a project from. */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
import * as React from 'react'

// =================
// === Templates ===
// =================

/** Define the template type to render. */
interface Template {
    title: string
    description?: string
    templateName?: string
}

/** Specifically refers to the creation of empty projects. */
const emptyTemplate = { title: 'New empty project' }
/** all type of project created */
const templates: Template[] = [
    emptyTemplate,
    {
        title: 'Colorado COVID',
        templateName: 'Colorado_COVID',
        description: 'Learn to glue multiple spreadsheets to analyses all your data at once.',
    },
    {
        title: 'KMeans',
        templateName: 'Kmeans',
        description: 'Learn where to open a coffee shop to maximize your income.',
    },
    {
        title: 'NASDAQ Returns',
        templateName: 'NASDAQ_Returns',
        description: 'Learn how to clean your data to prepare it for advanced analysis.',
    },
    {
        title: 'Restaurants',
        templateName: 'Orders',
        description: 'Learn how to clean your data to prepare it for advanced analysis.',
    },
    {
        title: 'Github Stars',
        templateName: 'Stargazers',
        description: 'Learn how to clean your data to prepare it for advanced analysis.',
    },
]

/** Temporary use of hard-coded background images. */
const dataUrl1 = `data:image/jpeg;base64,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`
const dataUrl2 = `data:image/jpeg;base64,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`
const genBackgroundImage = () => `url(${Math.random() > 0.5 ? dataUrl1 : dataUrl2})`

/** Render single template. */
interface TemplateRenderProps {
    template: Template
    onChange(name?: string): void
}
// eslint-disable-next-line @typescript-eslint/naming-convention
const TemplateRender: React.FC<TemplateRenderProps> = ({ template, onChange }) => {
    /** Unify the border color and the text color. */
    const borderColor = '9E8C91'
    /**
     * Dash border spacing is not supported by native CSS.
     * Therefore, use a background image to create the border.
     * It is essentially an SVG image that was generated by the website.
     * @see {@link https://kovart.github.io/dashed-border-generator}
     */
    const borderBgImg = `url("data:image/svg+xml,%3csvg width='100%25' height='100%25' xmlns='http://www.w3.org/2000/svg'%3e%3crect width='100%25' height='100%25' fill='none' rx='16' ry='16' stroke='%23${borderColor}FF' stroke-width='4' stroke-dasharray='15%2c 15' stroke-dashoffset='0' stroke-linecap='butt'/%3e%3c/svg%3e")`
    return (
        <button
            className="h-40 cursor-pointer"
            onClick={() => {
                onChange(template.templateName)
            }}
        >
            {template.templateName ? (
                <div
                    style={{ backgroundImage: genBackgroundImage() }}
                    className="flex flex-col justify-end h-full w-full rounded-2xl overflow-hidden text-white text-left bg-cover"
                >
                    <div className="bg-black bg-opacity-30 px-4 pt-2 pb-4">
                        <div className="text-sm font-bold pb-2">{template.title}</div>
                        <div className="text-xs h-[2lh] text-ellipsis">{template.description}</div>
                    </div>
                </div>
            ) : (
                <div
                    style={{ backgroundImage: borderBgImg }}
                    className={`flex h-full w-full rounded-2xl text-[#${borderColor}]`}
                >
                    <div className="m-auto text-center">
                        <button>
                            <svg
                                xmlns="http://www.w3.org/2000/svg"
                                fill="none"
                                viewBox="0 0 24 24"
                                strokeWidth={0.5}
                                stroke="currentColor"
                                className="w-20 h-20"
                            >
                                <path
                                    strokeLinecap="round"
                                    strokeLinejoin="round"
                                    d="M12 9v6m3-3H9m12 0a9 9 0 11-18 0 9 9 0 0118 0z"
                                />
                            </svg>
                        </button>
                        <p className="font-[600] text-sm">{template.title}</p>
                    </div>
                </div>
            )}
        </button>
    )
}

/** Render all templates. */
interface TemplatesProps {
    onChange(name?: string): void
}
// eslint-disable-next-line @typescript-eslint/naming-convention
const Templates: React.FC<TemplatesProps> = ({ onChange }) => {
    return (
        <div className="bg-white">
            <div className="mx-auto py-2 px-4 sm:py-4 sm:px-6 lg:px-8">
                <div className="grid grid-cols-1 gap-2 sm:grid-cols-3 lg:grid-cols-4 xl:grid-cols-5">
                    {templates.map(template => (
                        <TemplateRender
                            key={template.title}
                            template={template}
                            onChange={onChange}
                        />
                    ))}
                </div>
            </div>
        </div>
    )
}
export default Templates
