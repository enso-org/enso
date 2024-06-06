/** @file A modal to select labels for an asset. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as backendHooks from '#/hooks/backendHooks'

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

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import * as eventModule from '#/utilities/event'
import * as string from '#/utilities/string'

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
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/** A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManageLabelsModal<
  Asset extends backendModule.AnyAsset = backendModule.AnyAsset,
>(props: ManageLabelsModalProps<Asset>) {
  const { backend, item, eventTarget } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const labels = item.labels ?? []
  const allLabels = backendHooks.useBackendListTags(backend)
  const [query, setQuery] = React.useState('')
  const [color, setColor] = React.useState<backendModule.LChColor | null>(null)
  const leastUsedColor = React.useMemo(
    () => backendModule.leastUsedColor(allLabels ?? []),
    [allLabels]
  )
  const position = React.useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])
  const regex = React.useMemo(() => new RegExp(string.regexEscape(query), 'i'), [query])
  const canSelectColor = React.useMemo(
    () => query !== '' && (allLabels ?? []).filter(label => regex.test(label.value)).length === 0,
    [allLabels, query, regex]
  )
  const canCreateNewLabel = canSelectColor

  const createTagMutation = backendHooks.useBackendMutation(backend, 'createTag')
  const associateTagMutation = backendHooks.useBackendMutation(backend, 'associateTag')

  const doToggleLabel = (name: backendModule.LabelName) => {
    const newLabels = labels.includes(name)
      ? labels.filter(label => label !== name)
      : [...labels, name]
    associateTagMutation.mutate([item.id, newLabels])
  }

  const doSubmit = async () => {
    unsetModal()
    const labelName = backendModule.LabelName(query)
    await createTagMutation.mutateAsync([{ value: labelName, color: color ?? leastUsedColor }])
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
      >
        <div className="absolute h-full w-full rounded-default bg-selected-frame backdrop-blur-default" />
        <form
          className="relative flex flex-col gap-modal rounded-default p-modal"
          onSubmit={event => {
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
              {innerProps => (
                <div className="flex gap-input-with-button" {...innerProps}>
                  <FocusRing within>
                    <div
                      className={tailwindMerge.twMerge(
                        'flex grow items-center rounded-full border border-primary/10 px-input-x',
                        canSelectColor && color != null && color.lightness <= MAXIMUM_DARK_LIGHTNESS
                          ? 'text-tag-text placeholder-tag-text'
                          : 'text-primary'
                      )}
                      style={
                        !canSelectColor || color == null
                          ? {}
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
                        onChange={event => {
                          setQuery(event.currentTarget.value)
                        }}
                      />
                    </div>
                  </FocusRing>
                  <ariaComponents.Button
                    size="custom"
                    variant="custom"
                    isDisabled={!canCreateNewLabel}
                    className="button bg-invite px-button-x text-tag-text enabled:active"
                    onPress={eventModule.submitForm}
                  >
                    <aria.Text className="h-text py-modal-invite-button-text-y">
                      {getText('create')}
                    </aria.Text>
                  </ariaComponents.Button>
                </div>
              )}
            </FocusArea>
          }
          {canSelectColor && (
            <div className="mx-auto">
              <ColorPicker setColor={setColor} />
            </div>
          )}
          <FocusArea direction="vertical">
            {innerProps => (
              <div className="max-h-manage-labels-list overflow-auto" {...innerProps}>
                {(allLabels ?? [])
                  .filter(label => regex.test(label.value))
                  .map(label => (
                    <div key={label.id} className="flex h-row items-center">
                      <Label
                        active={labels.includes(label.value)}
                        color={label.color}
                        onPress={() => {
                          doToggleLabel(label.value)
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
