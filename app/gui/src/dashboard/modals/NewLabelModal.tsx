/** @file A modal for creating a new label. */
import * as React from 'react'

import { useMutation } from '@tanstack/react-query'
import * as z from 'zod'

import { ButtonGroup, Form, Input, Popover, Text } from '#/components/AriaComponents'
import ColorPicker from '#/components/ColorPicker'
import FocusArea from '#/components/styled/FocusArea'
import { backendMutationOptions, useBackendQuery } from '#/hooks/backendHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import { findLeastUsedColor } from '#/services/Backend'
import { EMPTY_ARRAY } from 'enso-common/src/utilities/data/array'

// =====================
// === NewLabelModal ===
// =====================

/** Props for a {@link NewLabelModal}. */
export interface NewLabelModalProps {
  readonly backend: Backend
}

/** A modal for creating a new label. */
export default function NewLabelModal(props: NewLabelModalProps) {
  const { backend } = props
  const { getText } = useText()
  const labels = useBackendQuery(backend, 'listTags', []).data ?? EMPTY_ARRAY
  const labelNames = React.useMemo(
    () => new Set<string>(labels.map((label) => label.value)),
    [labels],
  )
  const labelNamesRef = useSyncRef(labelNames)
  const leastUsedColor = React.useMemo(() => findLeastUsedColor(labels), [labels])

  const createTag = useMutation(backendMutationOptions(backend, 'createTag')).mutateAsync

  return (
    <Popover>
      <Form
        method="dialog"
        testId="new-label-modal"
        schema={z.object({
          name: z
            .string()
            .min(1)
            .refine((value) => !labelNamesRef.current.has(value), {
              message: getText('duplicateLabelError'),
            }),
          color: z.object({
            lightness: z.number(),
            chroma: z.number(),
            hue: z.number(),
            alpha: z.number().optional(),
          }),
        })}
        defaultValues={{ name: '', color: leastUsedColor }}
        onSubmit={async ({ name, color }) => {
          await createTag([{ value: name, color: color }])
        }}
      >
        {({ form }) => (
          <>
            <Text.Heading level={2} variant="subtitle">
              {getText('newLabel')}
            </Text.Heading>
            <Input
              form={form}
              name="name"
              label={getText('name')}
              autoFocus
              placeholder={getText('labelNamePlaceholder')}
            />
            <FocusArea direction="horizontal">
              {(innerProps) => (
                <ColorPicker
                  aria-label={getText('color')}
                  className="relative flex items-center"
                  pickerClassName="grow"
                  setColor={(color) => {
                    form.setValue('color', color)
                  }}
                  {...innerProps}
                >
                  <Text>{getText('color')}</Text>
                </ColorPicker>
              )}
            </FocusArea>
            <ButtonGroup className="relative">
              <Form.Submit>{getText('create')}</Form.Submit>
              <Form.Submit action="cancel" />
            </ButtonGroup>
            <Form.FormError />
          </>
        )}
      </Form>
    </Popover>
  )
}
