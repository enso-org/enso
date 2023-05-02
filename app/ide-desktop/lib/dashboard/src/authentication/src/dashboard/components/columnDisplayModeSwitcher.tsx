/** @file Switcher for choosing which columns are shown. */
import * as React from 'react'

import * as column from '../column'

// =================
// === Constants ===
// =================

// =================================
// === ColumnDisplayModeSwitcher ===
// =================================

export interface ColumnDisplayModeSwitcherProps {
    columnDisplayMode: column.ColumnDisplayMode
    setColumnDisplayMode: (columnDisplayMode: column.ColumnDisplayMode) => void
}

function ColumnDisplayModeSwitcher(props: ColumnDisplayModeSwitcherProps) {
    const { columnDisplayMode, setColumnDisplayMode } = props

    return (
        <>
            {column.COLUMN_DISPLAY_MODES_AND_NAMES.map(modeAndName => {
                const [mode, name] = modeAndName
                return (
                    <button
                        key={mode}
                        className={`${
                            columnDisplayMode === mode ? 'bg-white shadow-soft' : 'opacity-50'
                        } rounded-full px-1.5`}
                        onClick={() => {
                            setColumnDisplayMode(mode)
                        }}
                    >
                        {name}
                    </button>
                )
            })}
        </>
    )
}

export default ColumnDisplayModeSwitcher
