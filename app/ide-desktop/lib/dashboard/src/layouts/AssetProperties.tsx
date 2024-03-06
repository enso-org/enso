/** @file Display and modify the properties of an asset. */
import * as React from 'react'

import PenIcon from 'enso-assets/pen.svg'

import * as setAssetHooks from '#/hooks/setAssetHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'

import type * as assetEvent from '#/events/assetEvent'

import type Category from '#/layouts/CategorySwitcher/Category'

import Button from '#/components/Button'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'
import DataLinkInput from '#/components/dashboard/DataLinkInput'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import * as backendModule from '#/services/Backend'

import type AssetTreeNode from '#/utilities/AssetTreeNode'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'
import * as validateDataLink from '#/utilities/validateDataLink'

// =======================
// === AssetProperties ===
// =======================

/** Props for an {@link AssetPropertiesProps}. */
export interface AssetPropertiesProps {
  readonly item: AssetTreeNode
  readonly setItem: React.Dispatch<React.SetStateAction<AssetTreeNode>>
  readonly category: Category
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** Display and modify the properties of an asset. */
export default function AssetProperties(props: AssetPropertiesProps) {
  const { item: rawItem, setItem: rawSetItem, category, dispatchAssetEvent } = props

  const [item, innerSetItem] = React.useState(rawItem)
  const [isEditingDescription, setIsEditingDescription] = React.useState(false)
  const [queuedDescription, setQueuedDescripion] = React.useState<string | null>(null)
  const [description, setDescription] = React.useState('')
  const [dataLinkValue, setDataLinkValue] = React.useState<NonNullable<unknown> | null>(null)
  const [editedDataLinkValue, setEditedDataLinkValue] = React.useState<NonNullable<unknown> | null>(
    null
  )
  const [isDataLinkFetched, setIsDataLinkFetched] = React.useState(false)
  const isDataLinkSubmittable = React.useMemo(
    () => validateDataLink.validateDataLink(dataLinkValue),
    [dataLinkValue]
  )
  const { user } = authProvider.useNonPartialUserSession()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const setItem = React.useCallback(
    (valueOrUpdater: React.SetStateAction<AssetTreeNode>) => {
      innerSetItem(valueOrUpdater)
      rawSetItem(valueOrUpdater)
    },
    [/* should never change */ rawSetItem]
  )
  const smartAsset = item.item
  const asset = smartAsset.value
  const setAsset = setAssetHooks.useSetAsset(asset, setItem)
  const self = asset.permissions?.find(
    permission => permission.user.user_email === user?.value.email
  )
  const ownsThisAsset = self?.permission === permissions.PermissionAction.own
  const canEditThisAsset =
    ownsThisAsset ||
    self?.permission === permissions.PermissionAction.admin ||
    self?.permission === permissions.PermissionAction.edit
  const isDataLink = item.item.type === backendModule.AssetType.dataLink

  React.useEffect(() => {
    setDescription(asset.description ?? '')
  }, [asset.description])

  React.useEffect(() => {
    void (async () => {
      if (item.item.type === backendModule.AssetType.dataLink) {
        const value = await item.item.getValue()
        setDataLinkValue(value)
        setEditedDataLinkValue(structuredClone(value))
        setIsDataLinkFetched(true)
      }
    })()
  }, [item.item])

  const doEditDescription = async () => {
    setIsEditingDescription(false)
    if (description !== asset.description) {
      const oldDescription = asset.description
      setAsset(object.merger({ description }))
      try {
        await smartAsset.update({ description })
      } catch (error) {
        toastAndLog('Could not edit asset description')
        setAsset(object.merger({ description: oldDescription }))
      }
    }
  }

  return (
    <>
      <div className="flex flex-col items-start gap-1">
        <span className="flex items-center gap-2 text-lg leading-144.5 h-7 py-px">
          Description
          {ownsThisAsset && !isEditingDescription && (
            <Button
              image={PenIcon}
              onClick={() => {
                setIsEditingDescription(true)
                setQueuedDescripion(asset.description)
              }}
            />
          )}
        </span>
        <div data-testid="asset-panel-description" className="py-1 self-stretch">
          {!isEditingDescription ? (
            <span className="leading-170 py-px">{asset.description}</span>
          ) : (
            <form className="flex flex-col gap-2" onSubmit={doEditDescription}>
              <textarea
                ref={element => {
                  if (element != null && queuedDescription != null) {
                    element.value = queuedDescription
                    setQueuedDescripion(null)
                  }
                }}
                onBlur={doEditDescription}
                value={description}
                onKeyDown={event => {
                  event.stopPropagation()
                  switch (event.key) {
                    case 'Escape': {
                      setIsEditingDescription(false)
                      break
                    }
                    case 'Enter': {
                      if (event.ctrlKey) {
                        void doEditDescription()
                        break
                      }
                    }
                  }
                }}
                onChange={event => {
                  setDescription(event.currentTarget.value)
                }}
                className="bg-frame resize-none rounded-lg w-full p-2"
              />
              <button type="submit" className="self-start bg-frame-selected rounded-full px-4 py-1">
                Update
              </button>
            </form>
          )}
        </div>
      </div>
      <div className="flex flex-col items-start gap-2">
        <span className="text-lg leading-144.5 h-7 py-px">Settings</span>
        <table>
          <tbody>
            <tr data-testid="asset-panel-permissions">
              <td className="min-w-32 px-0 py-1">
                <span className="inline-block leading-170 h-6 py-px">Shared with</span>
              </td>
              <td className="p-0 w-full">
                <SharedWithColumn
                  item={item}
                  setItem={setItem}
                  state={{ category, dispatchAssetEvent }}
                />
              </td>
            </tr>
          </tbody>
        </table>
      </div>
      {isDataLink && (
        <div className="flex flex-col items-start gap-1">
          <span className="flex items-center gap-2 text-lg leading-144.5 h-7 py-px">Data Link</span>
          {!isDataLinkFetched ? (
            <div className="grid self-stretch place-items-center">
              <StatelessSpinner size={48} state={statelessSpinner.SpinnerState.loadingMedium} />
            </div>
          ) : (
            <>
              <DataLinkInput
                readOnly={!canEditThisAsset}
                dropdownTitle="Type"
                value={editedDataLinkValue}
                setValue={setEditedDataLinkValue}
              />
              {canEditThisAsset && (
                <div className="flex gap-2">
                  <button
                    type="button"
                    disabled={dataLinkValue === editedDataLinkValue || !isDataLinkSubmittable}
                    className="hover:cursor-pointer inline-block text-white bg-invite rounded-full px-4 py-1 disabled:opacity-50 disabled:cursor-default"
                    onClick={() => {
                      void (async () => {
                        if (item.item.type === backendModule.AssetType.dataLink) {
                          const oldDataLinkValue = dataLinkValue
                          try {
                            setDataLinkValue(editedDataLinkValue)
                            await item.item.update({ value: editedDataLinkValue })
                          } catch (error) {
                            toastAndLog(null, error)
                            setDataLinkValue(oldDataLinkValue)
                            setEditedDataLinkValue(oldDataLinkValue)
                          }
                        }
                      })()
                    }}
                  >
                    Update
                  </button>
                  <button
                    type="button"
                    className="hover:cursor-pointer inline-block bg-frame-selected rounded-full px-4 py-1"
                    onClick={() => {
                      setEditedDataLinkValue(structuredClone(dataLinkValue))
                    }}
                  >
                    Cancel
                  </button>
                </div>
              )}
            </>
          )}
        </div>
      )}
    </>
  )
}
