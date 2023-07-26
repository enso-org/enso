/** @file A selector for all possible permission types. */
import * as React from 'react'
import * as permissions from '../permissions'

// =================
// === Constants ===
// =================

/** Data for */
interface PermissionTypeData {
    type: permissions.Permission
    previous: permissions.Permission | null
    description: string
}

/** Data needed to display each permission type */
const PERMISSION_TYPE_DATA: PermissionTypeData[] = [
    {
        type: permissions.Permission.view,
        previous: null,
        description: 'File visibility only. Optionally, edit docs and execute project.',
    },
    {
        type: permissions.Permission.read,
        previous: permissions.Permission.view,
        description: 'File content reading.',
    },
    {
        type: permissions.Permission.edit,
        previous: permissions.Permission.read,
        description: 'File editing.',
    },
    {
        type: permissions.Permission.admin,
        previous: permissions.Permission.edit,
        description: 'Sharing management.',
    },
    {
        type: permissions.Permission.owner,
        previous: permissions.Permission.admin,
        description: 'File removal permission.',
    },
]

// ==============================
// === PermissionTypeSelector ===
// ==============================

/** Props for a {@link PermissionTypeSelector}. */
export interface PermissionTypeSelectorProps {
    type: permissions.Permission
    onChange: (permission: permissions.Permission) => void
}

/** A selector for all possible permission types. */
export default function PermissionTypeSelector(props: PermissionTypeSelectorProps) {
    const { type, onChange } = props
    // FIXME: it must be offset such that the first button is where the original button is
    return (
        <div className="relative">
            <div className="absolute">
                <div className="absolute bg-frame-selected rounded-2xl backdrop-blur-3xl w-full h-full -z-10" />
                <div className="flex flex-col w-109.75 p-1">
                    {PERMISSION_TYPE_DATA.map(data => (
                        <div
                            key={data.type}
                            className={`flex rounded-full gap-2 h-8 px-1 ${
                                type === data.type ? 'bg-black-a5' : ''
                            }`}
                        >
                            <button
                                className={`rounded-full w-13 h-5 my-1 py-0.5 ${
                                    permissions.PERMISSION_CLASS_NAME[data.type]
                                }`}
                                onClick={() => {
                                    onChange(data.type)
                                }}
                            >
                                {data.type}
                            </button>
                            <span className="font-normal leading-170 h-6.5 pt-1">
                                <span className="h-5.5 py-px">=</span>
                            </span>
                            {data.previous != null && (
                                <>
                                    <div
                                        className={`text-center rounded-full w-13 h-5 my-1 py-0.5 ${
                                            permissions.PERMISSION_CLASS_NAME[data.previous]
                                        }`}
                                    >
                                        {data.previous}
                                    </div>
                                    <span className="font-normal leading-170 h-6.5 pt-1">
                                        <span className="h-5.5 py-px">+</span>
                                    </span>
                                </>
                            )}
                            <div className="h-6.5 pt-1">
                                <span className="h-5.5 py-px">{data.description}</span>
                            </div>
                        </div>
                    ))}
                </div>
            </div>
        </div>
    )
}
