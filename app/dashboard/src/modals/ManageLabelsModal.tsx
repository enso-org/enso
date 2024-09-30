/** @file A modal to select labels for an asset. */
import { useEffect, useMemo, useState } from 'react'

import { useMutation } from '@tanstack/react-query'

import { Heading, Text } from '#/components/aria'
import { ButtonGroup, Checkbox, Form, Input } from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import Label from '#/components/dashboard/Label'
import Modal from '#/components/Modal'
import FocusArea from '#/components/styled/FocusArea'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useSetModal } from '#/providers/ModalProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { findLeastUsedColor, LabelName, type AnyAsset, type LChColor } from '#/services/Backend'
import { regexEscape } from '#/utilities/string'

// =========================
// === ManageLabelsModal ===
// =========================

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps<Asset extends AnyAsset = AnyAsset> {
  readonly backend: Backend
  readonly item: Asset
  /** If this is `null`, this modal will be centered. */
  readonly eventTarget: HTMLElement | null
}

/** A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case. */
export default function ManageLabelsModal<Asset extends AnyAsset = AnyAsset>(
  props: ManageLabelsModalProps<Asset>,
) {
  const { backend, item, eventTarget } = props
  const { unsetModal } = useSetModal()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const { data: allLabels } = useBackendQuery(backend, 'listTags', [])
  const [color, setColor] = useState<LChColor | null>(null)
  const leastUsedColor = useMemo(() => findLeastUsedColor(allLabels ?? []), [allLabels])
  const position = useMemo(() => eventTarget?.getBoundingClientRect(), [eventTarget])

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        labels: z.string().array().readonly(),
        name: z.string(),
      }),
    defaultValues: { labels: item.labels ?? [], name: '' },
    onSubmit: async ({ name }) => {
      const labelName = LabelName(name)
      try {
        await createTag([{ value: labelName, color: color ?? leastUsedColor }])
        unsetModal()
      } catch (error) {
        toastAndLog(null, error)
      }
    },
  })

  const formRef = useSyncRef(form)
  useEffect(() => {
    formRef.current.setValue('labels', item.labels ?? [])
  }, [formRef, item.labels])

  const query = Form.useWatch({ control: form.control, name: 'name' })
  const labels = Form.useWatch({ control: form.control, name: 'labels' })

  const regex = useMemo(() => new RegExp(regexEscape(query), 'i'), [query])
  const canSelectColor = useMemo(
    () => query !== '' && (allLabels ?? []).filter((label) => regex.test(label.value)).length === 0,
    [allLabels, query, regex],
  )
  const canCreateNewLabel = canSelectColor

  const createTag = useMutation(backendMutationOptions(backend, 'createTag')).mutateAsync
  const associateTag = useMutation(backendMutationOptions(backend, 'associateTag')).mutateAsync

  return (
    <Modal
      centered={eventTarget == null}
      className="absolute left top z-1 size-full overflow-hidden bg-dim"
    >
      <div
        tabIndex={-1}
        style={
          position != null ?
            { left: position.left + window.scrollX, top: position.top + window.scrollY }
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
        <Form form={form} className="relative flex flex-col gap-modal rounded-default p-modal">
          <Heading
            slot="title"
            level={2}
            className="flex h-row items-center gap-modal-tabs px-modal-tab-bar-x"
          >
            <Text className="text text-sm font-bold">{getText('labels')}</Text>
          </Heading>
          <FocusArea direction="horizontal">
            {(innerProps) => (
              <ButtonGroup className="relative" {...innerProps}>
                <Input
                  form={form}
                  name="name"
                  autoFocus
                  type="text"
                  size="small"
                  placeholder={getText('labelSearchPlaceholder')}
                />
                <Form.Submit isDisabled={!canCreateNewLabel}>{getText('create')}</Form.Submit>
              </ButtonGroup>
            )}
          </FocusArea>
          {canSelectColor && (
            <div className="mx-auto">
              <ColorPicker setColor={setColor} />
            </div>
          )}
          <FocusArea direction="vertical">
            {(innerProps) => (
              <Checkbox.Group
                form={form}
                name="labels"
                className="max-h-manage-labels-list overflow-auto"
                onChange={async (values) => {
                  await associateTag([item.id, values.map(LabelName), item.title])
                }}
                {...innerProps}
              >
                <>
                  {(allLabels ?? [])
                    .filter((label) => regex.test(label.value))
                    .map((label) => {
                      const isActive = labels.includes(label.value)
                      return (
                        <Checkbox key={label.id} value={String(label.value)} isSelected={isActive}>
                          <Label active={isActive} color={label.color} onPress={() => {}}>
                            {label.value}
                          </Label>
                        </Checkbox>
                      )
                    })}
                </>
              </Checkbox.Group>
            )}
          </FocusArea>
        </Form>
      </div>
    </Modal>
  )
}
