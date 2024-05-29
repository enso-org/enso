/** @file A modal for creating a new label. */
import * as React from 'react'

import * as tailwindMerge from 'tailwind-merge'

import * as backendHooks from '#/hooks/backendHooks'
import * as toastAndLogHooks from '#/hooks/toastAndLogHooks'

import * as modalProvider from '#/providers/ModalProvider'
import * as textProvider from '#/providers/TextProvider'

import * as aria from '#/components/aria'
import * as ariaComponents from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import Modal from '#/components/Modal'
import ButtonRow from '#/components/styled/ButtonRow'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

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
  const toastAndLog = toastAndLogHooks.useToastAndLog()
  const { unsetModal } = modalProvider.useSetModal()
  const { getText } = textProvider.useText()
  const [value, setValue] = React.useState('')
  const [color, setColor] = React.useState<backendModule.LChColor | null>(null)
  const position = React.useMemo(() => eventTarget.getBoundingClientRect(), [eventTarget])
  const labels = backendHooks.useBackendListTags(backend) ?? []
  const labelNames = React.useMemo(
    () => new Set<string>(labels.map(label => label.value)),
    [labels]
  )
  const leastUsedColor = React.useMemo(() => backendModule.leastUsedColor(labels), [labels])
  const canSubmit = Boolean(value && !labelNames.has(value))

  const createTagMutation = backendHooks.useBackendMutation(backend, 'createTag')

  const doSubmit = () => {
    if (value !== '') {
      unsetModal()
      try {
        createTagMutation.mutateAsync([{ value, color: color ?? leastUsedColor }])
      } catch (error) {
        toastAndLog(null, error)
      }
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
        onClick={event => {
          event.stopPropagation()
        }}
        onSubmit={event => {
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
          {innerProps => (
            <aria.TextField className="relative flex items-center" {...innerProps}>
              <aria.Label className="text w-modal-label">{getText('name')}</aria.Label>
              <FocusRing>
                <aria.Input
                  autoFocus
                  size={1}
                  placeholder={getText('labelNamePlaceholder')}
                  className={tailwindMerge.twMerge(
                    'focus-child text grow rounded-full border border-primary/10 bg-transparent px-input-x',
                    color != null && color.lightness <= MAXIMUM_DARK_LIGHTNESS
                      ? 'text-tag-text placeholder-selected-frame'
                      : 'text-primary'
                  )}
                  style={
                    color == null
                      ? {}
                      : { backgroundColor: backendModule.lChColorToCssColor(color) }
                  }
                  onInput={event => {
                    setValue(event.currentTarget.value)
                  }}
                />
              </FocusRing>
            </aria.TextField>
          )}
        </FocusArea>
        <FocusArea direction="horizontal">
          {innerProps => (
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
        <ButtonRow>
          <ariaComponents.Button
            size="custom"
            variant="custom"
            isDisabled={!canSubmit}
            className="button bg-invite text-white enabled:active"
            onPress={doSubmit}
          >
            {getText('create')}
          </ariaComponents.Button>
          <ariaComponents.Button
            size="custom"
            variant="custom"
            className="button bg-selected-frame active"
            onPress={unsetModal}
          >
            {getText('cancel')}
          </ariaComponents.Button>
        </ButtonRow>
      </form>
    </Modal>
  )
}
