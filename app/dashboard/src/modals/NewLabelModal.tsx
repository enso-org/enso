/** @file A modal for creating a new label. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'

import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import Modal from '#/components/Modal'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'

import * as tailwindMerge from '#/utilities/tailwindMerge'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'

// =================
// === Constants ===
// =================

/** The maximum lightness at which a color is still considered dark. */
const MAXIMUM_DARK_LIGHTNESS = 50

// =====================
// === NewLabelModal ===
// =====================

/** Props for a {@link NewLabelModal}. */
export interface NewLabelModalProps {
  readonly backend: Backend
  readonly eventTarget: HTMLElement
}

/** A modal for creating a new label. */
export default function NewLabelModal(props: NewLabelModalProps) {
  const { backend, eventTarget } = props
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [value, setValue] = React.useState('')
  const [color, setColor] = React.useState<backendModule.LChColor | null>(null)
  const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
  const labels = useBackendQuery(backend, 'listTags', []).data ?? EMPTY_ARRAY
  const labelNames = React.useMemo(
    () => new Set<string>(labels.map((label) => label.value)),
    [labels],
  )
  const leastUsedColor = React.useMemo(() => backendModule.leastUsedColor(labels), [labels])
  const canSubmit = Boolean(value && !labelNames.has(value))

  const createTag = useMutation(backendMutationOptions(backend, 'createTag')).mutate

  const doSubmit = () => {
    if (value !== '') {
      unsetModal()
      createTag([{ value, color: color ?? leastUsedColor }])
    }
  }

  return (
    <Modal className="absolute bg-dim">
      <form
        data-testid="new-label-modal"
        tabIndex={-1}
        style={{
          left: position.left + window.scrollX,
          top: position.top + window.scrollY,
        }}
        className="pointer-events-auto relative flex w-new-label-modal flex-col gap-modal rounded-default p-modal-wide pt-modal before:absolute before:inset before:h-full before:w-full before:rounded-default before:bg-selected-frame before:backdrop-blur-default"
        onClick={(event) => {
          event.stopPropagation()
        }}
        onSubmit={(event) => {
          event.preventDefault()
          // Consider not calling `onSubmit()` here to make it harder to accidentally
          // delete an important asset.
          doSubmit()
        }}
      >
        <aria.Heading level={2} className="relative text-sm font-semibold">
          {getText('newLabel')}
        </aria.Heading>
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <aria.TextField className="relative flex items-center" {...innerProps}>
              <aria.Label className="text w-modal-label">{getText('name')}</aria.Label>
              <FocusRing>
                <aria.Input
                  autoFocus
                  size={1}
                  placeholder={getText('labelNamePlaceholder')}
                  className={tailwindMerge.twMerge(
                    'focus-child text grow rounded-full border border-primary/10 bg-transparent px-input-x',
                    color != null && color.lightness <= MAXIMUM_DARK_LIGHTNESS ?
                      'text-tag-text placeholder-selected-frame'
                    : 'text-primary',
                  )}
                  style={
                    color == null ?
                      {}
                    : { backgroundColor: backendModule.lChColorToCssColor(color) }
                  }
                  onInput={(event) => {
                    setValue(event.currentTarget.value)
                  }}
                />
              </FocusRing>
            </aria.TextField>
          )}
        </FocusArea>
        <FocusArea direction="horizontal">
          {(innerProps) => (
            <ColorPicker
              className="relative flex items-center"
              pickerClassName="grow"
              setColor={setColor}
              {...innerProps}
            >
              <aria.Label className="text w-modal-label">{getText('color')}</aria.Label>
            </ColorPicker>
          )}
        </FocusArea>
        <ariaComponents.ButtonGroup className="relative">
          <ariaComponents.Button variant="submit" isDisabled={!canSubmit} onPress={doSubmit}>
            {getText('create')}
          </ariaComponents.Button>
          <ariaComponents.Button variant="outline" onPress={unsetModal}>
            {getText('cancel')}
          </ariaComponents.Button>
        </ariaComponents.ButtonGroup>
      </form>
    </Modal>
  )
}
