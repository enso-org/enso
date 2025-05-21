/** @file A modal to select labels for an asset. */
import { useState } from 'react'

import { useQuery } from '@tanstack/react-query'

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
import FocusRing from '#/components/styled/FocusRing'
import { backendMutationOptions, backendQueryOptions } from '#/hooks/backendHooks'
import { useToastAndLog } from '#/hooks/toastAndLogHooks'
import { useAsset } from '#/layouts/Drive/assetsTableItemsHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import type Backend from '#/services/Backend'
import { findLeastUsedColor, LabelName, type AnyAsset, type LChColor } from '#/services/Backend'
import { regexEscape } from '#/utilities/string'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'

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
      <ManageLabelsForm {...props} />
    </Popover>
  )
}

/**
 * Form for {@link ManageLabelsModal}.
 */
function ManageLabelsForm(props: ManageLabelsModalProps) {
  const { backend, item: itemRaw } = props

  const item = useAsset(itemRaw.id) ?? itemRaw

  const [id, setId] = useState(0)
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const { data: allLabels = [] } = useQuery(backendQueryOptions(backend, 'listTags', []))
  const [color, setColor] = useState<LChColor | null>(null)
  const leastUsedColor = findLeastUsedColor(allLabels)

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
    query !== '' && allLabels.every((label) => label.value.toLowerCase() !== query.toLowerCase())
  const canCreateNewLabel = canSelectColor

  return (
    <Form key={id} form={form}>
      <Text.Heading slot="title" level={2} variant="subtitle">
        {getText('labels')}
      </Text.Heading>
      <ButtonGroup className="relative">
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
      {canSelectColor && <ColorPicker setColor={setColor} className="w-full" />}
      <Checkbox.Group
        form={form}
        name="labels"
        fullWidth
        className="max-h-80 overflow-auto"
        onChange={async (values) => {
          await associateTag([item.id, values.map(LabelName), item.title])
        }}
      >
        {allLabels
          .filter((label) => regex.test(label.value))
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
                      icon="trash"
                      extraClickZone={false}
                      aria-label={getText('delete')}
                      tooltipPlacement="right"
                      showIconOnHover
                      className="relative mr-1 flex size-4 text-delete transition-all after:absolute after:-inset-1 after:rounded-button-focus-ring group-has-[[data-focus-visible]]:active group-hover:active"
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
    </Form>
  )
}
