/** @file A modal to select labels for an asset. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions, useListTags } from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import Label from '#/components/dashboard/Label'
import Modal from '#/components/Modal'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import Input from '#/components/styled/Input'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as object from '#/utilities/object'
import * as string from '#/utilities/string'
import * as tailwindMerge from '#/utilities/tailwindMerge'

// =================
// === Constants ===
// =================

/** The maximum lightness at which a color is still considered dark. */
const MAXIMUM_DARK_LIGHTNESS = 50

// =========================
// === ManageLabelsModal ===
// =========================

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
> {
  readonly backend: Backend
  readonly item: Asset
  readonly setItem: React.Dispatch<React.SetStateAction<Asset>>
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/** A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManageLabelsModal<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
>(props: ManageLabelsModalProps<Asset>) {
  const { backend, item, setItem, eventTarget } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const allLabels = useListTags(backend)
  const [labels, setLabelsRaw] = React.useState(item.labels ?? [])
  const [query, setQuery] = React.useState('')
  const [color, setColor] = React.useState<backendModule.LChColor | null>(null)
  const leastUsedColor = React.useMemo(
    () => backendModule.leastUsedColor(allLabels ?? []),
    [allLabels],
  )
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const labelNames = React.useMemo(() => new Set(labels), [labels])
  const regex = React.useMemo(() => new RegExp(string.regexEscape(query), 'i'), [query])
  const canSelectColor = React.useMemo(
    () => query !== '' && (allLabels ?? []).filter((label) => regex.test(label.value)).length === 0,
    [allLabels, query, regex],
  )
  const canCreateNewLabel = canSelectColor

  const createTag = useMutation(backendMutationOptions(backend, 'createTag')).mutateAsync
  const associateTag = useMutation(backendMutationOptions(backend, 'associateTag')).mutateAsync

  const setLabels = React.useCallback(
    (valueOrUpdater: React.SetStateAction<readonly backendModule.LabelName[]>) => {
      setLabelsRaw(valueOrUpdater)
      setItem((oldItem) =>
        // This is SAFE, as the type of asset is not being changed.
        // eslint-disable-next-line no-restricted-syntax
        object.merge(oldItem, {
          labels:
            typeof valueOrUpdater !== 'function' ? valueOrUpdater : (
              valueOrUpdater(oldItem.labels ?? [])
            ),
        } as Partial<Asset>),
      )
    },
    [setItem],
  )

  const doToggleLabel = async (name: backendModule.LabelName) => {
    const newLabels =
      labelNames.has(name) ? labels.filter((label) => label !== name) : [...labels, name]
    setLabels(newLabels)
    try {
      await associateTag([item.id, newLabels, item.title])
    } catch (error) {
      toastAndLog(null, error)
      setLabels(labels)
    }
  }

  const doSubmit = async () => {
    unsetModal()
    const labelName = backendModule.LabelName(query)
    setLabels((oldLabels) => [...oldLabels, labelName])
    try {
      await createTag([{ value: labelName, color: color ?? leastUsedColor }])
      setLabels((newLabels) => {
        void associateTag([item.id, newLabels, item.title])
        return newLabels
      })
    } catch (error) {
      toastAndLog(null, error)
      setLabels((oldLabels) => oldLabels.filter((oldLabel) => oldLabel !== query))
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
          position != null ?
            {
              left: position.left + window.scrollX,
              top: position.top + window.scrollY,
            }
          : {}
        }
        className="sticky w-manage-labels-modal"
        onClick={(mouseEvent) => {
          mouseEvent.stopPropagation()
        }}
        onContextMenu={(mouseEvent) => {
          mouseEvent.stopPropagation()
          mouseEvent.preventDefault()
        }}
      >
        <div className="absolute h-full w-full rounded-default bg-selected-frame backdrop-blur-default" />
        <form
          className="relative flex flex-col gap-modal rounded-default p-modal"
          onSubmit={(event) => {
            event.preventDefault()
            void doSubmit()
          }}
        >
          <aria.Heading
            level={2}
            className="flex h-row items-center gap-modal-tabs px-modal-tab-bar-x"
          >
            <aria.Text className="text text-sm font-bold">{getText('labels')}</aria.Text>
          </aria.Heading>
          {
            <FocusArea direction="horizontal">
              {(innerProps) => (
                <ariaComponents.ButtonGroup className="relative" {...innerProps}>
                  <FocusRing within>
                    <div
                      className={tailwindMerge.twMerge(
                        'flex grow items-center rounded-full border border-primary/10 px-input-x',
                        (
                          canSelectColor &&
                            color != null &&
                            color.lightness <= MAXIMUM_DARK_LIGHTNESS
                        ) ?
                          'text-tag-text placeholder-tag-text'
                        : 'text-primary',
                      )}
                      style={
                        !canSelectColor || color == null ?
                          {}
                        : {
                            backgroundColor: backendModule.lChColorToCssColor(color),
                          }
                      }
                    >
                      <Input
                        autoFocus
                        type="text"
                        size={1}
                        placeholder={getText('labelSearchPlaceholder')}
                        className="text grow bg-transparent"
                        onChange={(event) => {
                          setQuery(event.currentTarget.value)
                        }}
                      />
                    </div>
                  </FocusRing>
                  <ariaComponents.Button
                    variant="submit"
                    isDisabled={!canCreateNewLabel}
                    onPress={eventModule.submitForm}
                  >
                    {getText('create')}
                  </ariaComponents.Button>
                </ariaComponents.ButtonGroup>
              )}
            </FocusArea>
          }
          {canSelectColor && (
            <div className="mx-auto">
              <ColorPicker setColor={setColor} />
            </div>
          )}
          <FocusArea direction="vertical">
            {(innerProps) => (
              <div className="max-h-manage-labels-list overflow-auto" {...innerProps}>
                {(allLabels ?? [])
                  .filter((label) => regex.test(label.value))
                  .map((label) => (
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
            )}
          </FocusArea>
        </form>
      </div>
    </Modal>
  )
}
