/** @file A modal to select labels for an asset. */
import { useState } from 'react'

import {
  Button,
  ButtonGroup,
  Checkbox,
  DialogTrigger,
  Form,
  Input,
  Popover,
  Text,
} from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import Label from '#/components/dashboard/Label'
import FocusArea from '#/components/styled/FocusArea'
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { findLeastUsedColor, LabelName, type AnyAsset, type LChColor } from '#/services/Backend'
import { regexEscape } from '#/utilities/string'
import { useMutationCallback } from '#/utilities/tanstackQuery'

/** Props for a {@link ManageLabelsModal}. */
export interface ManageLabelsModalProps<Asset extends AnyAsset = AnyAsset> {
  readonly backend: Backend
  readonly item: Asset
  readonly triggerRef?: React.MutableRefObject<HTMLElement | null>
}

/**
 * A modal to select labels for an asset.
 * @throws {Error} when the current backend is the local backend, or when the user is offline.
 * This should never happen, as this modal should not be accessible in either case.
 */
export default function ManageLabelsModal<Asset extends AnyAsset = AnyAsset>(
  props: ManageLabelsModalProps<Asset>,
) {
  const { triggerRef } = props
  return (
    <Popover
      size="xsmall"
      {...(triggerRef ? { triggerRef } : {})}
      shouldCloseOnInteractOutside={() => true}
    >
      <ManageLabelsModalInternal {...props} />
    </Popover>
  )
}

/**
 * Internal implementation of a {@link ManageLabelsModal}.
 */
function ManageLabelsModalInternal(props: ManageLabelsModalProps) {
  const { backend, item: itemRaw } = props

  const item = useAsset(itemRaw.id) ?? itemRaw

  const [id, setId] = useState(0)
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const { data: allLabels } = useBackendQuery(backend, 'listTags', [])
  const [color, setColor] = useState<LChColor | null>(null)
  const leastUsedColor = findLeastUsedColor(allLabels ?? [])

  const createTag = useMutationCallback(backendMutationOptions(backend, 'createTag'))
  const associateTag = useMutationCallback(backendMutationOptions(backend, 'associateTag'))
  const deleteTag = useMutationCallback(backendMutationOptions(backend, 'deleteTag'))

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
        const newLabels = [...(item.labels ?? []), labelName]
        await associateTag([item.id, newLabels, item.title])
        form.resetField('labels', { defaultValue: newLabels })
        setId((currentId) => currentId + 1)
      } catch (error) {
        toastAndLog(null, error)
      }
    },
  })

  const query = Form.useWatch({ control: form.control, name: 'name' })
  const labels = Form.useWatch({ control: form.control, name: 'labels' })

  const regex = new RegExp(regexEscape(query), 'i')
  const canSelectColor =
    query !== '' &&
    (allLabels ?? []).every((label) => label.value.toLowerCase() !== query.toLowerCase())
  const canCreateNewLabel = canSelectColor

  return (
    <Form key={id} form={form} className="relative flex flex-col gap-modal rounded-default p-modal">
      <Text.Heading slot="title" level={2} variant="subtitle">
        {getText('labels')}
      </Text.Heading>
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
      {canSelectColor && <ColorPicker setColor={setColor} className="w-full" />}
      <FocusArea direction="vertical">
        {(innerProps) => (
          <Checkbox.Group
            form={form}
            name="labels"
            fullWidth
            className="max-h-80 overflow-auto"
            onChange={async (values) => {
              await associateTag([item.id, values.map(LabelName), item.title])
            }}
            {...innerProps}
          >
            {allLabels
              ?.filter((label) => regex.test(label.value))
              .map((label) => {
                const isActive = labels.includes(label.value)
                return (
                  <div className="group flex w-full items-center justify-between">
                    <Checkbox key={label.id} value={String(label.value)}>
                      <Label active={isActive} color={label.color} onPress={() => {}}>
                        {label.value}
                      </Label>
                    </Checkbox>

                    <FocusRing placement="after">
                      <DialogTrigger>
                        <Button
                          variant="icon"
                          icon="trash2"
                          extraClickZone={false}
                          aria-label={getText('delete')}
                          tooltipPlacement="right"
                          className="relative mr-1 flex size-4 text-delete opacity-0 transition-all after:absolute after:-inset-1 after:rounded-button-focus-ring group-has-[[data-focus-visible]]:active group-hover:active"
                        />
                        <ConfirmDeleteModal
                          cannotUndo
                          actionText={getText('deleteLabelActionText', label.value)}
                          onConfirm={async () => {
                            await deleteTag([label.id, label.value])
                          }}
                        />
                      </DialogTrigger>
                    </FocusRing>
                  </div>
                )
              })}
          </Checkbox.Group>
        )}
      </FocusArea>
    </Form>
  )
}
