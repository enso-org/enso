/** @file A selector for all possible permission types. */
import * as React from 'react'
import * as permissions from '../permissions'

// =================
// === Constants ===
// =================

/** Data for */
interface PermissionTypeData {
    type: permissions.Permission
    description: string
}

/** Data needed to display each permission type */
const PERMISSION_TYPE_DATA: PermissionTypeData[] = [
    {
        type: permissions.Permission.view,
        description:
            'Can see the file but not read it. Can also be allowed to edit documentation and execute the project.',
    },
    {
        type: permissions.Permission.read,
        description:
            'Can read the file. Can also be allowed to edit documentation or execute the project.',
    },
    {
        type: permissions.Permission.edit,
        description: 'Can edit the file, its documentation, and execute the project.',
    },
    {
        type: permissions.Permission.admin,
        description:
            'Can see the files. Optionally, can be assigned with permissions to edit documentation or execute the project.',
    },
    {
        type: permissions.Permission.owner,
        description:
            'Can see the files. Optionally, can be assigned with permissions to edit documentation or execute the project.',
    },
]

// ==============================
// === PermissionTypeSelector ===
// ==============================

/** Props for a {@link PermissionTypeSelector}. */
export interface PermissionTypeSelectorProps {
    onChange: (permission: permissions.Permission) => void
}

/** A selector for all possible permission types. */
export default function PermissionTypeSelector(props: PermissionTypeSelectorProps) {
    const { onChange } = props
    // FIXME: it must be offset such that the first button is where the original button is
    return (
        <div className="flex flex-col gap-2 px-4 pt-2 pb-3">
            {PERMISSION_TYPE_DATA.map(data => (
                <div key={data.type} className="gap-1.5">
                    <button
                        className={`rounded-full w-30.25 h-5 my-1 py-0.5 ${
                            permissions.PERMISSION_CLASS_NAME[data.type]
                        }`}
                        onClick={() => {
                            onChange(data.type)
                        }}
                    >
                        {data.type}
                    </button>
                    <span className="pt-1">{data.description}</span>
                </div>
            ))}
        </div>
    )
}
