/** @file Display and modify the properties of an asset. */
import * as React from 'react'

import PenIcon from 'enso-assets/pen.svg'

import * as datalinkValidator from '#/data/datalinkValidator'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as authProvider from '#/providers/AuthProvider'
import * as textProvider from '#/providers/TextProvider'

import type * as assetEvent from '#/events/assetEvent'

import type Category from '#/layouts/CategorySwitcher/Category'

import * as aria from '#/components/aria'
import SharedWithColumn from '#/components/dashboard/column/SharedWithColumn'
import DatalinkInput from '#/components/dashboard/DatalinkInput'
import Label from '#/components/dashboard/Label'
import StatelessSpinner, * as statelessSpinner from '#/components/StatelessSpinner'
import Button from '#/components/styled/Button'
import UnstyledButton from '#/components/UnstyledButton'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import type AssetQuery from '#/utilities/AssetQuery'
import type * as assetTreeNode from '#/utilities/AssetTreeNode'
import * as object from '#/utilities/object'
import * as permissions from '#/utilities/permissions'

// =======================
// === AssetProperties ===
// =======================

/** Props for an {@link AssetPropertiesProps}. */
export interface AssetPropertiesProps {
  readonly backend: Backend
  readonly item: assetTreeNode.AnyAssetTreeNode
  readonly setItem: React.Dispatch<React.SetStateAction<assetTreeNode.AnyAssetTreeNode>>
  readonly category: Category
  readonly labels: backendModule.Label[]
  readonly setQuery: React.Dispatch<React.SetStateAction<AssetQuery>>
  readonly dispatchAssetEvent: (event: assetEvent.AssetEvent) => void
  readonly isReadonly?: boolean
}

/** Display and modify the properties of an asset. */
export default function AssetProperties(props: AssetPropertiesProps) {
  const { backend, item: itemRaw, setItem: setItemRaw, category, labels, setQuery } = props
  const { isReadonly = false, dispatchAssetEvent } = props

  const { user } = authProvider.useNonPartialUserSession()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [item, setItemInner] = React.useState(itemRaw)
  const [isEditingDescription, setIsEditingDescription] = React.useState(false)
  const [queuedDescription, setQueuedDescripion] = React.useState<string | null>(null)
  const [description, setDescription] = React.useState('')
  const [datalinkValue, setDatalinkValue] = React.useState<NonNullable<unknown> | null>(null)
  const [editedDatalinkValue, setEditedDatalinkValue] = React.useState<NonNullable<unknown> | null>(
    datalinkValue
  )
  const [isDatalinkFetched, setIsDatalinkFetched] = React.useState(false)
  const isDatalinkSubmittable = React.useMemo(
    () => datalinkValidator.validateDatalink(datalinkValue),
    [datalinkValue]
  )
  const setItem = React.useCallback(
    (valueOrUpdater: React.SetStateAction<assetTreeNode.AnyAssetTreeNode>) => {
      setItemInner(valueOrUpdater)
      setItemRaw(valueOrUpdater)
    },
    [/* should never change */ setItemRaw]
  )
  const self = item.item.permissions?.find(
    backendModule.isUserPermissionAnd(permission => permission.user.userId === user?.userId)
  )
  const ownsThisAsset = self?.permission === permissions.PermissionAction.own
  const canEditThisAsset =
    ownsThisAsset ||
    self?.permission === permissions.PermissionAction.admin ||
    self?.permission === permissions.PermissionAction.edit
  const isDatalink = item.item.type === backendModule.AssetType.datalink
  const isDatalinkDisabled = datalinkValue === editedDatalinkValue || !isDatalinkSubmittable

  React.useEffect(() => {
    setDescription(item.item.description ?? '')
  }, [item.item.description])

  React.useEffect(() => {
    void (async () => {
      if (item.item.type === backendModule.AssetType.datalink) {
        const value = await backend.getDatalink(item.item.id, item.item.title)
        setDatalinkValue(value)
        setEditedDatalinkValue(value)
        setIsDatalinkFetched(true)
      }
    })()
  }, [backend, item.item])

  const doEditDescription = async () => {
    setIsEditingDescription(false)
    if (description !== item.item.description) {
      const oldDescription = item.item.description
      setItem(oldItem => oldItem.with({ item: object.merge(oldItem.item, { description }) }))
      try {
        const projectPath = item.item.projectState?.path
        await backend.updateAsset(
          item.item.id,
          {
            parentDirectoryId: null,
            description,
            ...(projectPath == null ? {} : { projectPath }),
          },
          item.item.title
        )
      } catch (error) {
        toastAndLog('editDescriptionError')
        setItem(oldItem =>
          oldItem.with({ item: object.merge(oldItem.item, { description: oldDescription }) })
        )
      }
    }
  }

  return (
    <>
      <div className="pointer-events-auto flex flex-col items-start gap-side-panel">
        <aria.Heading
          level={2}
          className="flex h-side-panel-heading items-center gap-side-panel-section py-side-panel-heading-y text-lg leading-snug"
        >
          {getText('description')}
          {!isReadonly && ownsThisAsset && !isEditingDescription && (
            <Button
              image={PenIcon}
              onPress={() => {
                setIsEditingDescription(true)
                setQueuedDescripion(item.item.description)
              }}
            />
          )}
        </aria.Heading>
        <div
          data-testid="asset-panel-description"
          className="self-stretch py-side-panel-description-y"
        >
          {!isEditingDescription ? (
            <aria.Text className="text">{item.item.description}</aria.Text>
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
                <UnstyledButton
                  className="button self-start bg-selected-frame"
                  onPress={doEditDescription}
                >
                  {getText('update')}
                </UnstyledButton>
              </div>
            </form>
          )}
        </div>
      </div>
      <div className="pointer-events-auto flex flex-col items-start gap-side-panel-section">
        <aria.Heading
          level={2}
          className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
        >
          {getText('settings')}
        </aria.Heading>
        <table>
          <tbody>
            <tr data-testid="asset-panel-permissions" className="h-row">
              <td className="text my-auto min-w-side-panel-label p">
                <aria.Label className="text inline-block">{getText('sharedWith')}</aria.Label>
              </td>
              <td className="w-full p">
                <SharedWithColumn
                  isReadonly={isReadonly}
                  item={item}
                  setItem={setItem}
                  state={{ backend, category, dispatchAssetEvent, setQuery }}
                />
              </td>
            </tr>
            <tr data-testid="asset-panel-labels" className="h-row">
              <td className="text my-auto min-w-side-panel-label p">
                <aria.Label className="text inline-block">{getText('labels')}</aria.Label>
              </td>
              <td className="w-full p">
                {item.item.labels?.map(value => {
                  const label = labels.find(otherLabel => otherLabel.value === value)
                  return label == null ? null : (
                    <Label key={value} active isDisabled color={label.color} onPress={() => {}}>
                      {value}
                    </Label>
                  )
                })}
              </td>
            </tr>
          </tbody>
        </table>
      </div>
      {isDatalink && (
        <div className="pointer-events-auto flex flex-col items-start gap-side-panel-section">
          <aria.Heading
            level={2}
            className="h-side-panel-heading py-side-panel-heading-y text-lg leading-snug"
          >
            {getText('datalink')}
          </aria.Heading>
          {!isDatalinkFetched ? (
            <div className="grid place-items-center self-stretch">
              <StatelessSpinner size={48} state={statelessSpinner.SpinnerState.loadingMedium} />
            </div>
          ) : (
            <>
              <DatalinkInput
                readOnly={!canEditThisAsset}
                dropdownTitle="Type"
                value={editedDatalinkValue}
                setValue={setEditedDatalinkValue}
              />
              {canEditThisAsset && (
                <div className="flex gap-buttons">
                  <UnstyledButton
                    isDisabled={isDatalinkDisabled}
                    {...(isDatalinkDisabled
                      ? { title: 'Edit the Datalink before updating it.' }
                      : {})}
                    className="button bg-invite text-white enabled:active"
                    onPress={() => {
                      void (async () => {
                        if (item.item.type === backendModule.AssetType.datalink) {
                          const oldDatalinkValue = datalinkValue
                          try {
                            setDatalinkValue(editedDatalinkValue)
                            await backend.createDatalink({
                              datalinkId: item.item.id,
                              name: item.item.title,
                              parentDirectoryId: null,
                              value: editedDatalinkValue,
                            })
                          } catch (error) {
                            toastAndLog(null, error)
                            setDatalinkValue(oldDatalinkValue)
                            setEditedDatalinkValue(oldDatalinkValue)
                          }
                        }
                      })()
                    }}
                  >
                    {getText('update')}
                  </UnstyledButton>
                  <UnstyledButton
                    isDisabled={isDatalinkDisabled}
                    className="button bg-selected-frame enabled:active"
                    onPress={() => {
                      setEditedDatalinkValue(datalinkValue)
                    }}
                  >
                    {getText('cancel')}
                  </UnstyledButton>
                </div>
              )}
            </>
          )}
        </div>
      )}
    </>
  )
}
