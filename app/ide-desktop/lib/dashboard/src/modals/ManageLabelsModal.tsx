/** @file A modal to select labels for an asset. */
import * as React from 'react'

import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as backendProvider from '#/providers/BackendProvider'
import * as modalProvider from '#/providers/ModalProvider'

import * as aria from '#/components/aria'
import ColorPicker from '#/components/ColorPicker'
import Label from '#/components/dashboard/Label'
import Modal from '#/components/Modal'
import UnstyledButton from '#/components/styled/UnstyledButton'

import * as backendModule from '#/services/Backend'

import * as object from '#/utilities/object'
import * as string from '#/utilities/string'

// =========================
// === ManageLabelsModal ===
// =========================

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
> {
  readonly item: Asset
  readonly setItem: React.Dispatch<React.SetStateAction<Asset>>
  readonly allLabels: Map<backendModule.LabelName, backendModule.Label>
  readonly doCreateLabel: (value: string, color: backendModule.LChColor) => Promise<void>
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/** A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManageLabelsModal<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
>(props: ManageLabelsModalProps<Asset>) {
  const { item, setItem, allLabels, doCreateLabel, eventTarget } = props
  const { backend } = backendProvider.useBackend()
  const { unsetModal } = modalProvider.useSetModal()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const [labels, setLabelsRaw] = React.useState(item.labels ?? [])
  const [query, setQuery] = React.useState('')
  const [color, setColor] = React.useState<backendModule.LChColor | null>(null)
  const leastUsedColor = React.useMemo(
    () => backendModule.leastUsedColor(allLabels.values()),
    [allLabels]
  )
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const labelNames = React.useMemo(() => new Set(labels), [labels])
  const regex = React.useMemo(() => new RegExp(string.regexEscape(query), 'i'), [query])
  const canSelectColor = React.useMemo(
    () =>
      query !== '' && Array.from(allLabels.keys()).filter(label => regex.test(label)).length === 0,
    [allLabels, query, regex]
  )
  const canCreateNewLabel = canSelectColor

  const setLabels = React.useCallback(
    (valueOrUpdater: React.SetStateAction<backendModule.LabelName[]>) => {
      setLabelsRaw(valueOrUpdater)
      setItem(oldItem =>
        // This is SAFE, as the type of asset is not being changed.
        // eslint-disable-next-line no-restricted-syntax
        object.merge(oldItem, {
          labels:
            typeof valueOrUpdater !== 'function'
              ? valueOrUpdater
              : valueOrUpdater(oldItem.labels ?? []),
        } as Partial<Asset>)
      )
    },
    [/* should never change */ setItem]
  )

  const doToggleLabel = async (name: backendModule.LabelName) => {
    const newLabels = labelNames.has(name)
      ? labels.filter(label => label !== name)
      : [...labels, name]
    setLabels(newLabels)
    try {
      await backend.associateTag(item.id, newLabels, item.title)
    } catch (error) {
      toastAndLog(null, error)
      setLabels(labels)
    }
  }

  const doSubmit = async () => {
    unsetModal()
    setLabels(oldLabels => [...oldLabels, backendModule.LabelName(query)])
    try {
      await doCreateLabel(query, color ?? leastUsedColor)
      setLabels(newLabels => {
        void backend.associateTag(item.id, newLabels, item.title)
        return newLabels
      })
    } catch (error) {
      toastAndLog(null, error)
      setLabels(oldLabels => oldLabels.filter(oldLabel => oldLabel !== query))
    }
  }

  return (
    <Modal
      centered={eventTarget == null}
      className="absolute left top z-1 size-full overflow-hidden bg-dim"
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
        className="sticky w-manage-labels-modal"
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
        <div className="absolute h-full w-full rounded-default bg-selected-frame backdrop-blur-default" />
        <form
          className="relative flex flex-col gap-modal rounded-default p-modal"
          onSubmit={event => {
            event.preventDefault()
            void doSubmit()
          }}
        >
          <div className="flex h-row items-center gap-modal-tabs px-modal-tab-bar-x">
            <h2 className="text text-sm font-bold">Labels</h2>
          </div>
          <div className="flex gap-input-with-button">
            <div
              className={`flex grow items-center rounded-full border border-primary/10 px-input-x ${
                // eslint-disable-next-line @typescript-eslint/no-magic-numbers
                canSelectColor && color != null && color.lightness <= 50
                  ? 'text-tag-text placeholder-tag-text'
                  : 'text-primary'
              }`}
              style={
                !canSelectColor || color == null
                  ? {}
                  : {
                      backgroundColor: backendModule.lChColorToCssColor(color),
                    }
              }
            >
              <aria.Input
                autoFocus
                type="text"
                size={1}
                placeholder="Type labels to search"
                className="text grow bg-transparent"
                onChange={event => {
                  setQuery(event.currentTarget.value)
                }}
              />
            </div>
            <UnstyledButton
              isDisabled={!canCreateNewLabel}
              className="button bg-invite px-button-x text-tag-text enabled:active"
              onPress={doSubmit}
            >
              <aria.Text className="h-text py-modal-invite-button-text-y">Create</aria.Text>
            </UnstyledButton>
          </div>
          {canSelectColor && (
            <div className="mx-auto">
              <ColorPicker setColor={setColor} />
            </div>
          )}
          <div className="max-h-manage-labels-list overflow-auto">
            {Array.from(allLabels.values())
              .filter(label => regex.test(label.value))
              .map(label => (
                <div key={label.id} className="flex h-row items-center">
                  <Label
                    active={labels.includes(label.value)}
                    color={label.color}
                    onPress={() => {
                      void doToggleLabel(label.value)
                    }}
                  >
                    {label.value}
                  </Label>
                </div>
              ))}
          </div>
        </form>
      </div>
    </Modal>
  )
}
