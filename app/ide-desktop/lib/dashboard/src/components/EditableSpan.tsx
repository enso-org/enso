/** @file A text `<span>` which turns into an `input` when desired. */
import * as React from 'react'

import CrossIcon from 'enso-assets/cross.svg'
import TickIcon from 'enso-assets/tick.svg'

import * as shortcutManagerProvider from '#/providers/ShortcutManagerProvider'
import * as shortcutManagerModule from '#/utilities/ShortcutManager'

// ====================
// === EditableSpan ===
// ====================

/** Props of an {@link EditableSpan} that are passed through to the base element. */
type EditableSpanPassthroughProps = JSX.IntrinsicElements['input'] & JSX.IntrinsicElements['span']

/** Props for an {@link EditableSpan}. */
export interface EditableSpanProps extends Omit<EditableSpanPassthroughProps, 'onSubmit'> {
    editable?: boolean
    checkSubmittable?: (value: string) => boolean
    onSubmit: (value: string) => void
    onCancel: () => void
    inputPattern?: string
    inputTitle?: string
    children: string
}

/** A `<span>` that can turn into an `<input type="text">`. */
export default function EditableSpan(props: EditableSpanProps) {
    const {
        editable = false,
        checkSubmittable,
        children,
        onSubmit,
        onCancel,
        inputPattern,
        inputTitle,
        ...passthrough
    } = props
    const { shortcutManager } = shortcutManagerProvider.useShortcutManager()
    const [isSubmittable, setIsSubmittable] = React.useState(true)
    const inputRef = React.useRef<HTMLInputElement>(null)

    React.useEffect(() => {
        setIsSubmittable(checkSubmittable?.(inputRef.current?.value ?? '') ?? true)
        // This effect MUST only run on mount.
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [])

    React.useEffect(() => {
        if (editable) {
            return shortcutManager.registerKeyboardHandlers({
                [shortcutManagerModule.KeyboardAction.cancelEditName]: () => {
                    onCancel()
                    inputRef.current?.blur()
                },
            })
        } else {
            return
        }
    }, [editable, shortcutManager, onCancel])

    if (editable) {
        return (
            <form
                className="flex grow"
                onSubmit={event => {
                    event.preventDefault()
                    if (isSubmittable) {
                        if (inputRef.current != null) {
                            onSubmit(inputRef.current.value)
                        }
                    }
                }}
            >
                <input
                    ref={inputRef}
                    autoFocus
                    type="text"
                    size={1}
                    defaultValue={children}
                    onBlur={event => event.currentTarget.form?.requestSubmit()}
                    {...(inputPattern == null ? {} : { pattern: inputPattern })}
                    {...(inputTitle == null ? {} : { title: inputTitle })}
                    {...(checkSubmittable == null
                        ? {}
                        : {
                              onInput: event => {
                                  setIsSubmittable(checkSubmittable(event.currentTarget.value))
                              },
                          })}
                    {...passthrough}
                />
                {isSubmittable && (
                    <button type="submit" className="mx-0.5">
                        <img src={TickIcon} alt="Confirm Edit" />
                    </button>
                )}
                <button
                    type="button"
                    className="mx-0.5"
                    onClick={event => {
                        event.stopPropagation()
                        onCancel()
                    }}
                >
                    <img src={CrossIcon} alt="Cancel Edit" />
                </button>
            </form>
        )
    } else {
        return <span {...passthrough}>{children}</span>
    }
}
