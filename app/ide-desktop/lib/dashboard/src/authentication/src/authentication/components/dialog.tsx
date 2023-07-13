/** @file A styled, centered full-screen dialog. */

import * as React from 'react'

// ==============
// === Dialog ===
// ==============

/** Props for a {@link Dialog}. */
export interface DialogProps extends React.PropsWithChildren {
    title: string
    onSubmit: (event: React.FormEvent<HTMLFormElement>) => Promise<void> | void
    header?: JSX.Element
    footer?: JSX.Element
}

/** A styled, centered full-screen dialog. */
function Dialog(props: DialogProps) {
    const { title, onSubmit, header, footer, children } = props
    return (
        <div className="text-primary text-xs flex flex-col items-center justify-center bg-primary bg-opacity-30 min-h-screen">
            <div className="flex flex-col bg-primary-bg shadow-soft rounded-2xl gap-6 p-4 w-full max-w-sm">
                <div className="font-semibold self-center text-lg">{title}</div>
                {header}
                <form className="flex flex-col gap-3" onSubmit={onSubmit}>
                    {children}
                </form>
                {footer}
            </div>
        </div>
    )
}

export default Dialog
