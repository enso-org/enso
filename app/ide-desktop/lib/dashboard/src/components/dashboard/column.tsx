/** @file Column types and column display modes. */
import * as React from 'react'

import Plus2Icon from 'enso-assets/plus2.svg'

import * as events from '#/events'
import * as hooks from '#/hooks'
import type * as assetsTable from '#/layouts/dashboard/assetsTable'
import * as categorySwitcherConstants from '#/layouts/dashboard/categorySwitcher/categorySwitcherConstants'
import ManageLabelsModal from '#/layouts/dashboard/manageLabelsModal'
import ManagePermissionsModal from '#/layouts/dashboard/managePermissionsModal'
import * as providers from '#/providers'
import type * as backendModule from '#/services/backend'
import type * as assetTreeNode from '#/util/assetTreeNode'
import * as dateTime from '#/util/dateTime'
import * as permissions from '#/util/permissions'
import * as shortcuts from '#/util/shortcuts'
import * as uniqueString from '#/util/uniqueString'

import ContextMenu from '#/components/contextMenu'
import ContextMenus from '#/components/contextMenus'
import AssetNameColumn from '#/components/dashboard/assetNameColumn'
import * as columnUtils from '#/components/dashboard/column/columnUtils'
import Label from '#/components/dashboard/label'
import * as labelUtils from '#/components/dashboard/label/labelUtils'
import PermissionDisplay from '#/components/dashboard/permissionDisplay'
import MenuEntry from '#/components/menuEntry'
import type * as tableColumn from '#/components/tableColumn'

// ==========================
// === LastModifiedColumn ===
// ==========================

/** {@link tableColumn.TableColumnProps} for an unknown variant of {@link backendModule.Asset}. */
export type AssetColumnProps = tableColumn.TableColumnProps<
    assetTreeNode.AssetTreeNode,
    assetsTable.AssetsTableState,
    assetsTable.AssetRowState,
    backendModule.AssetId
>

/** A column displaying the time at which the asset was last modified. */
function LastModifiedColumn(props: AssetColumnProps) {
    return <>{dateTime.formatDateTime(new Date(props.item.item.modifiedAt))}</>
}

// ========================
// === SharedWithColumn ===
// ========================

/** The type of the `state` prop of a {@link SharedWithColumn}. */
interface SharedWithColumnStateProp {
    category: AssetColumnProps['state']['category']
    dispatchAssetEvent: AssetColumnProps['state']['dispatchAssetEvent']
}

/** Props for a {@link SharedWithColumn}. */
interface SharedWithColumnPropsInternal extends Pick<AssetColumnProps, 'item' | 'setItem'> {
    state: SharedWithColumnStateProp
}

/** A column listing the users with which this asset is shared. */
function SharedWithColumn(props: SharedWithColumnPropsInternal) {
    const {
        item: { item: asset },
        setItem,
        state: { category, dispatchAssetEvent },
    } = props
    const session = providers.useNonPartialUserSession()
    const { setModal } = providers.useSetModal()
    const [isHovered, setIsHovered] = React.useState(false)
    const self = asset.permissions?.find(
        permission => permission.user.user_email === session.organization?.email
    )
    const managesThisAsset =
        category !== categorySwitcherConstants.Category.trash &&
        (self?.permission === permissions.PermissionAction.own ||
            self?.permission === permissions.PermissionAction.admin)
    const setAsset = React.useCallback(
        (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
            if (typeof valueOrUpdater === 'function') {
                setItem(oldItem => ({
                    ...oldItem,
                    item: valueOrUpdater(oldItem.item),
                }))
            } else {
                setItem(oldItem => ({ ...oldItem, item: valueOrUpdater }))
            }
        },
        [/* should never change */ setItem]
    )
    return (
        <div
            className="flex items-center gap-1"
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
        >
            {(asset.permissions ?? []).map(user => (
                <PermissionDisplay key={user.user.pk} action={user.permission}>
                    {user.user.user_name}
                </PermissionDisplay>
            ))}
            {managesThisAsset && (
                <button
                    className={`h-4 w-4 ${isHovered ? '' : 'invisible pointer-events-none'}`}
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManagePermissionsModal
                                key={uniqueString.uniqueString()}
                                item={asset}
                                setItem={setAsset}
                                self={self}
                                eventTarget={event.currentTarget}
                                doRemoveSelf={() => {
                                    dispatchAssetEvent({
                                        type: events.AssetEventType.removeSelf,
                                        id: asset.id,
                                    })
                                }}
                            />
                        )
                    }}
                >
                    <img className="w-4.5 h-4.5" src={Plus2Icon} />
                </button>
            )}
        </div>
    )
}

// ====================
// === LabelsColumn ===
// ====================

/** A column listing the labels on this asset. */
function LabelsColumn(props: AssetColumnProps) {
    const {
        item: { item: asset },
        setItem,
        state: { category, labels, setQuery, deletedLabelNames, doCreateLabel },
        rowState: { temporarilyAddedLabels, temporarilyRemovedLabels },
    } = props
    const session = providers.useNonPartialUserSession()
    const { setModal, unsetModal } = providers.useSetModal()
    const { backend } = providers.useBackend()
    const toastAndLog = hooks.useToastAndLog()
    const [isHovered, setIsHovered] = React.useState(false)
    const self = asset.permissions?.find(
        permission => permission.user.user_email === session.organization?.email
    )
    const managesThisAsset =
        category !== categorySwitcherConstants.Category.trash &&
        (self?.permission === permissions.PermissionAction.own ||
            self?.permission === permissions.PermissionAction.admin)
    const setAsset = React.useCallback(
        (valueOrUpdater: React.SetStateAction<backendModule.AnyAsset>) => {
            if (typeof valueOrUpdater === 'function') {
                setItem(oldItem => ({
                    ...oldItem,
                    item: valueOrUpdater(oldItem.item),
                }))
            } else {
                setItem(oldItem => ({ ...oldItem, item: valueOrUpdater }))
            }
        },
        [/* should never change */ setItem]
    )
    return (
        <div
            className="flex items-center gap-1"
            onMouseEnter={() => {
                setIsHovered(true)
            }}
            onMouseLeave={() => {
                setIsHovered(false)
            }}
        >
            {(asset.labels ?? [])
                .filter(label => !deletedLabelNames.has(label))
                .map(label => (
                    <Label
                        key={label}
                        color={labels.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
                        active={!temporarilyRemovedLabels.has(label)}
                        disabled={temporarilyRemovedLabels.has(label)}
                        className={
                            temporarilyRemovedLabels.has(label)
                                ? 'relative before:absolute before:rounded-full before:border-2 before:border-delete before:inset-0 before:w-full before:h-full'
                                : ''
                        }
                        onContextMenu={event => {
                            event.preventDefault()
                            event.stopPropagation()
                            const doDelete = () => {
                                unsetModal()
                                setAsset(oldAsset => {
                                    const newLabels =
                                        oldAsset.labels?.filter(oldLabel => oldLabel !== label) ??
                                        []
                                    void backend
                                        .associateTag(asset.id, newLabels, asset.title)
                                        .catch(error => {
                                            toastAndLog(null, error)
                                            setAsset(oldAsset2 =>
                                                oldAsset2.labels?.some(
                                                    oldLabel => oldLabel === label
                                                ) === true
                                                    ? oldAsset2
                                                    : {
                                                          ...oldAsset2,
                                                          labels: [
                                                              ...(oldAsset2.labels ?? []),
                                                              label,
                                                          ],
                                                      }
                                            )
                                        })
                                    return {
                                        ...oldAsset,
                                        labels: newLabels,
                                    }
                                })
                            }
                            setModal(
                                <ContextMenus key={`label-${label}`} event={event}>
                                    <ContextMenu>
                                        <MenuEntry
                                            action={shortcuts.KeyboardAction.delete}
                                            doAction={doDelete}
                                        />
                                    </ContextMenu>
                                </ContextMenus>
                            )
                        }}
                        onClick={event => {
                            event.preventDefault()
                            event.stopPropagation()
                            setQuery(oldQuery =>
                                oldQuery.labels.includes(label)
                                    ? oldQuery.delete({ labels: [label] })
                                    : oldQuery.add({ labels: [label] })
                            )
                        }}
                    >
                        {label}
                    </Label>
                ))}
            {...[...temporarilyAddedLabels]
                .filter(label => asset.labels?.includes(label) !== true)
                .map(label => (
                    <Label
                        disabled
                        key={label}
                        color={labels.get(label)?.color ?? labelUtils.DEFAULT_LABEL_COLOR}
                        className="pointer-events-none"
                        onClick={() => {}}
                    >
                        {label}
                    </Label>
                ))}
            {managesThisAsset && (
                <button
                    className={`h-4 w-4 ${isHovered ? '' : 'invisible pointer-events-none'}`}
                    onClick={event => {
                        event.stopPropagation()
                        setModal(
                            <ManageLabelsModal
                                key={uniqueString.uniqueString()}
                                item={asset}
                                setItem={setAsset}
                                allLabels={labels}
                                doCreateLabel={doCreateLabel}
                                eventTarget={event.currentTarget}
                            />
                        )
                    }}
                >
                    <img className="w-4.5 h-4.5" src={Plus2Icon} />
                </button>
            )}
        </div>
    )
}

/** A column listing the users with which this asset is shared. */
function DocsColumn(props: AssetColumnProps) {
    const {
        item: { item: asset },
    } = props
    return <div className="flex items-center gap-1">{asset.description}</div>
}

// =========================
// === PlaceholderColumn ===
// =========================

/** A placeholder component for columns which do not yet have corresponding data to display. */
function PlaceholderColumn() {
    return <></>
}

/** React components for every column except for the name column. */
// This is not a React component even though it contains JSX.
// eslint-disable-next-line no-restricted-syntax, @typescript-eslint/no-unused-vars
export const COLUMN_RENDERER: Record<columnUtils.Column, (props: AssetColumnProps) => JSX.Element> =
    {
        [columnUtils.Column.name]: AssetNameColumn,
        [columnUtils.Column.modified]: LastModifiedColumn,
        [columnUtils.Column.sharedWith]: SharedWithColumn,
        [columnUtils.Column.labels]: LabelsColumn,
        [columnUtils.Column.accessedByProjects]: PlaceholderColumn,
        [columnUtils.Column.accessedData]: PlaceholderColumn,
        [columnUtils.Column.docs]: DocsColumn,
    }
