/** @file A modal to select labels for an asset. */
import * as React from 'react'

import * as auth from '../../authentication/providers/auth'
import * as backendModule from '../backend'
import * as backendProvider from '../../providers/backend'
import * as hooks from '../../hooks'
import * as string from '../../string'

import Label from './label'
import Modal from './modal'

// =========================
// === ManageLabelsModal ===
// =========================

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps<
    Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
> {
    item: Asset
    setItem: React.Dispatch<React.SetStateAction<Asset>>
    self: backendModule.UserPermission
    allLabels: backendModule.LabelName[]
    /** If this is `null`, this modal will be centered. */
    eventTarget: HTMLElement | null
}

/** A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManageLabelsModal<
    Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
>(props: ManageLabelsModalProps<Asset>) {
    const { item, setItem, allLabels, eventTarget } = props
    const { organization } = auth.useNonPartialUserSession()
    const { backend } = backendProvider.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [labels, setLabels] = React.useState(item.labels ?? [])
    const [query, setQuery] = React.useState('')
    const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
    const labelNames = React.useMemo(() => new Set(labels), [labels])
    const regex = React.useMemo(() => new RegExp(string.regexEscape(query), 'i'), [query])
    const canCreateNewLabel = React.useMemo(
        () => query !== '' && allLabels.filter(label => regex.test(label)).length === 0,
        [allLabels, query, regex]
    )

    React.useEffect(() => {
        setItem(oldItem => ({ ...oldItem, labels }))
    }, [labels, /* should never change */ setItem])

    if (backend.type === backendModule.BackendType.local || organization == null) {
        // This should never happen - the local backend does not have the "labels" column,
        // and `organization` is absent only when offline - in which case the user should only
        // be able to access the local backend.
        // This MUST be an error, otherwise the hooks below are considered as conditionally called.
        throw new Error('Cannot add labels to assets on the local backend.')
    } else {
        const doToggleLabel = React.useCallback(
            async (name: backendModule.LabelName) => {
                const newLabels = labelNames.has(name)
                    ? labels.filter(label => label !== name)
                    : [...labels, name]
                setLabels(newLabels)
                try {
                    await backend.associateTag(item.id, newLabels, item.title)
                } catch (error) {
                    toastAndLog(null, error)
                }
            },
            [
                labelNames,
                labels,
                item.id,
                item.title,
                backend,
                /* should never change */ toastAndLog,
            ]
        )

        return (
            <Modal
                centered={eventTarget == null}
                className="absolute overflow-hidden bg-dim w-full h-full top-0 left-0 z-1"
            >
                <div
                    tabIndex={-1}
                    style={
                        position != null
                            ? {
                                  left: position.left + window.scrollX,
                                  top: position.top + window.scrollY,
                              }
                            : {}
                    }
                    className="sticky w-60"
                    onClick={mouseEvent => {
                        mouseEvent.stopPropagation()
                    }}
                    onContextMenu={mouseEvent => {
                        mouseEvent.stopPropagation()
                        mouseEvent.preventDefault()
                    }}
                    onKeyDown={event => {
                        if (event.key !== 'Escape') {
                            event.stopPropagation()
                        }
                    }}
                >
                    <div className="absolute bg-frame-selected backdrop-blur-3xl rounded-2xl h-full w-full" />
                    <div className="relative flex flex-col rounded-2xl gap-2 p-2">
                        <div>
                            <h2 className="text-sm font-bold">Labels</h2>
                            {/* Space reserved for other tabs. */}
                        </div>
                        <form className="flex gap-1">
                            <div className="flex items-center grow rounded-full border border-black-a10 gap-2 px-1">
                                <input
                                    autoFocus
                                    type="text"
                                    placeholder="Type labels to search"
                                    className="grow bg-transparent leading-170 h-6 py-px"
                                    onChange={event => {
                                        setQuery(event.currentTarget.value)
                                    }}
                                />
                            </div>
                            <button
                                type="submit"
                                disabled={!canCreateNewLabel}
                                className="text-tag-text bg-invite rounded-full px-2 py-1 disabled:opacity-30"
                            >
                                <div className="h-6 py-0.5">Create</div>
                            </button>
                        </form>
                        <div className="overflow-auto pl-1 pr-12 max-h-80">
                            {allLabels
                                .filter(label => regex.test(label))
                                .map(label => (
                                    <div key={label} className="flex items-center h-8">
                                        <Label
                                            onClick={async () => {
                                                await doToggleLabel(label)
                                            }}
                                        />
                                    </div>
                                ))}
                        </div>
                    </div>
                </div>
            </Modal>
        )
    }
}
