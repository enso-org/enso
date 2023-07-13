/** @file A styled submit button. */
import * as React from 'react'

/** Props for a {@link SubmitButton}. */
export interface SubmitButtonProps extends React.PropsWithChildren {}

/** A styled submit button. */
function SubmitButton(props: SubmitButtonProps) {
    const { children } = props
    return (
        <div className="flex w-full">
            <button
                type="submit"
                className="text-sm flex items-center justify-center text-white bg-blue-600 hover:bg-blue-700 rounded-full gap-2 py-2 w-full transition duration-150 ease-in"
            >
                {children}
            </button>
        </div>
    )
}

export default SubmitButton
