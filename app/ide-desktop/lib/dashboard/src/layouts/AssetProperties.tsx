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
import Label from '#/components/dashboard/Label'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'

import * as backendModule from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'
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
  readonly labels: backendModule.Label[]
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
}

/** Display and modify the properties of an asset. */
export default function AssetProperties(props: AssetPropertiesProps) {
  const { item: itemRaw, setItem: setItemRaw, category, labels, setQuery } = props
  const { dispatchAssetEvent } = props

  const [item, setItemInner] = React.useState(itemRaw)
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
      setItemInner(valueOrUpdater)
      setItemRaw(valueOrUpdater)
    },
    [/* should never change */ setItemRaw]
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
      <div className="flex flex-col items-start gap-side-panel">
        <span className="flex h-side-panel-heading items-center gap-side-panel-section py-side-panel-heading-y text-lg leading-snug">
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
        <div
          data-testid="asset-panel-description"
          className="self-stretch py-side-panel-description-y"
        >
          {!isEditingDescription ? (
            <span className="text">{asset.description}</span>
          ) : (
            <form className="flex flex-col gap-modal" onSubmit={doEditDescription}>
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
                className="-m-multiline-input-p w-full resize-none rounded-input bg-frame p-multiline-input"
              />
              <div className="flex gap-buttons">
                <button type="submit" className="button self-start bg-selected-frame">
                  Update
                </button>
              </div>
            </form>
          )}
        </div>
      </div>
      <div className="flex flex-col items-start gap-side-panel-section">
        <h2 className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug">
          Settings
        </h2>
        <table>
          <tbody>
            <tr data-testid="asset-panel-permissions" className="h-row">
              <td className="text my-auto min-w-side-panel-label p">
                <span className="text inline-block">Shared with</span>
              </td>
              <td className="w-full p">
                <SharedWithColumn
                  item={item}
                  setItem={setItem}
                  state={{ category, dispatchAssetEvent, setQuery }}
                />
              </td>
            </tr>
            <tr data-testid="asset-panel-labels" className="h-row">
              <td className="text my-auto min-w-side-panel-label p">
                <span className="text inline-block">Labels</span>
              </td>
              <td className="w-full p">
                {asset.labels?.map(value => {
                  const label = labels.find(otherLabel => otherLabel.value === value)
                  return label == null ? null : (
                    <Label key={value} active disabled color={label.color} onClick={() => {}}>
                      {value}
                    </Label>
                  )
                })}
              </td>
            </tr>
          </tbody>
        </table>
      </div>
      {isDataLink && (
        <div className="flex flex-col items-start gap-side-panel-section">
          <h2 className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug">
            Data Link
          </h2>
          {!isDataLinkFetched ? (
            <div className="grid place-items-center self-stretch">
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
                <div className="flex gap-buttons">
                  <button
                    type="button"
                    disabled={dataLinkValue === editedDataLinkValue || !isDataLinkSubmittable}
                    className="button bg-invite text-white selectable enabled:active"
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
                    className="button bg-selected-frame"
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
