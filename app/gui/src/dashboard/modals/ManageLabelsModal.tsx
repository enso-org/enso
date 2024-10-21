/** @file A modal to select labels for an asset. */
import { useEffect, useMemo, useState } from 'react'

import { useMutation } from '@tanstack/react-query'

import { ButtonGroup, Checkbox, Form, Input, Popover, Text } from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import Label from '#/components/dashboard/Label'
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
  const { backend, item, triggerRef } = props
  const { unsetModal } = useSetModal()
  const { getText } = useText()
  const toastAndLog = useToastAndLog()
  const { data: allLabels } = useBackendQuery(backend, 'listTags', [])
  const [color, setColor] = useState<LChColor | null>(null)
  const leastUsedColor = useMemo(() => findLeastUsedColor(allLabels ?? []), [allLabels])

  const createTagMutation = useMutation(backendMutationOptions(backend, 'createTag'))
  const associateTagMutation = useMutation(backendMutationOptions(backend, 'associateTag'))

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
        await createTagMutation.mutateAsync([{ value: labelName, color: color ?? leastUsedColor }])
        await associateTagMutation.mutateAsync([
          item.id,
          [...(item.labels ?? []), labelName],
          item.title,
        ])
        unsetModal()
      } catch (error) {
        toastAndLog(null, error)
      }
    },
  })

  const formRef = useSyncRef(form)
  useEffect(() => {
    formRef.current.resetField('labels', { defaultValue: item.labels ?? [] })
  }, [formRef, item.labels])

  const query = Form.useWatch({ control: form.control, name: 'name' })
  const labels = Form.useWatch({ control: form.control, name: 'labels' })

  const regex = useMemo(() => new RegExp(regexEscape(query), 'i'), [query])
  const canSelectColor = useMemo(
    () => query !== '' && (allLabels ?? []).filter((label) => regex.test(label.value)).length === 0,
    [allLabels, query, regex],
  )
  const canCreateNewLabel = canSelectColor

  return (
    <Popover size="xsmall" {...(triggerRef ? { triggerRef } : {})}>
      <Form form={form} className="relative flex flex-col gap-modal rounded-default p-modal">
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
              className="max-h-manage-labels-list overflow-auto"
              onChange={async (values) => {
                await associateTagMutation.mutateAsync([item.id, values.map(LabelName), item.title])
              }}
              {...innerProps}
            >
              <>
                {allLabels
                  ?.filter((label) => regex.test(label.value))
                  .map((label) => {
                    const isActive = labels.includes(label.value)
                    return (
                      <Checkbox key={label.id} value={String(label.value)}>
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
    </Popover>
  )
}
