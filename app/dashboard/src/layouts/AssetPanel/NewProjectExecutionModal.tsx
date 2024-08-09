/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import type Backend from '#/services/Backend'
import {
  PARALLEL_MODE_TO_TEXT_ID,
  PROJECT_PARALLEL_MODES,
  PROJECT_REPEAT_INTERVALS,
  REPEAT_INTERVAL_TO_TEXT_ID,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'

import {
  Button,
  ButtonGroup,
  Dialog,
  Form,
  Input,
  MultiSelector,
  Selector,
} from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useText, type GetText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'
import { DAY_3_LETTER_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'
import { omit } from 'enso-common/src/utilities/data/object'

/* eslint-disable @typescript-eslint/no-magic-numbers */
const DATES: readonly number[] = [...Array(31).keys()]
const DAYS: readonly number[] = [...Array(7).keys()]
const HOURS: readonly number[] = [...Array(24).keys()]

/** Create the form schema for this page. */
function createUpsertExecutionSchema(getText: GetText) {
  const schema = z.object({
    repeatInterval: z.enum(PROJECT_REPEAT_INTERVALS),
    dates: z
      .number()
      .int()
      .min(0)
      .max(30)
      .array()
      .transform((arr) => arr.sort((a, b) => a - b))
      .readonly()
      .optional(),
    days: z
      .number()
      .int()
      .min(0)
      .max(6)
      .array()
      .transform((arr) => arr.sort((a, b) => a - b))
      .readonly()
      .optional(),
    hours: z
      .number()
      .int()
      .min(0)
      .max(23)
      .array()
      .transform((arr) => arr.sort((a, b) => a - b))
      .readonly()
      .optional(),
    minute: z.number().int().min(0).max(59),
    parallelMode: z.enum(PROJECT_PARALLEL_MODES),
  })
  /** The base schema for this object. */
  type Schema = z.infer<typeof schema>
  return schema
    .superRefine((object, context) => {
      switch (object.repeatInterval) {
        case 'hourly': {
          // No action needed.
          break
        }
        case 'daily': {
          if (!object.hours || object.hours.length === 0) {
            context.addIssue({
              code: 'custom',
              path: ['hours'],
              message: getText('pleaseSelectAtLeastOneItem'),
            })
          }
          break
        }
        case 'weekly': {
          if (!object.days || object.days.length === 0) {
            context.addIssue({
              code: 'custom',
              path: ['days'],
              message: getText('pleaseSelectAtLeastOneItem'),
            })
          }
          if (!object.hours || object.hours.length === 0) {
            context.addIssue({
              code: 'custom',
              path: ['hours'],
              message: getText('pleaseSelectAtLeastOneItem'),
            })
          }
          break
        }
        case 'monthly': {
          if (!object.dates || object.dates.length === 0) {
            context.addIssue({
              code: 'custom',
              path: ['dates'],
              message: getText('pleaseSelectAtLeastOneItem'),
            })
          }
          if (!object.hours || object.hours.length === 0) {
            context.addIssue({
              code: 'custom',
              path: ['hours'],
              message: getText('pleaseSelectAtLeastOneItem'),
            })
          }
          break
        }
      }
    })
    .transform<Schema>((object) => {
      switch (object.repeatInterval) {
        case 'hourly': {
          return omit(object, 'hours', 'dates', 'days')
        }
        case 'daily': {
          return omit(object, 'dates', 'days')
        }
        case 'weekly': {
          return omit(object, 'dates')
        }
        case 'monthly': {
          return omit(object, 'days')
        }
      }
    })
}
/* eslint-enable @typescript-eslint/no-magic-numbers */

// ================================
// === NewProjectExecutionModal ===
// ================================

/** Props for a {@link NewProjectExecutionModal}. */
export interface NewProjectExecutionModalProps {
  readonly backend: Backend
  readonly item: ProjectAsset
}

/** A modal for confirming the deletion of an asset. */
export default function NewProjectExecutionModal(props: NewProjectExecutionModalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const form = Form.useForm({ schema: createUpsertExecutionSchema(getText) })

  const repeatInterval = form.watch('repeatInterval', 'weekly')

  const createProjectExecution = useMutation(
    backendMutationOptions(backend, 'createProjectExecution'),
  ).mutateAsync

  return (
    <Dialog title={getText('newProjectExecution')}>
      {({ close }) => (
        <Form
          form={form}
          method="dialog"
          defaultValues={{ repeatInterval: 'weekly', parallelMode: 'restart', minute: 0 }}
          className="w-full"
          onSubmit={async (values) => {
            const { repeatInterval: newRepeatInterval, parallelMode, ...times } = values
            await createProjectExecution([
              {
                projectId: item.id,
                repeatInterval: newRepeatInterval,
                times,
                parallelMode,
              },
              item.title,
            ])
          }}
        >
          <Selector
            form={form}
            name="repeatInterval"
            label={getText('repeatIntervalLabel')}
            items={PROJECT_REPEAT_INTERVALS}
            itemToString={(interval) => getText(REPEAT_INTERVAL_TO_TEXT_ID[interval])}
          />
          {repeatInterval === 'monthly' && (
            <MultiSelector
              form={form}
              isRequired
              name="dates"
              label={getText('datesLabel')}
              items={DATES}
              itemToString={(n) => String(n + 1)}
              columns={10}
            />
          )}
          {repeatInterval === 'weekly' && (
            <MultiSelector
              form={form}
              isRequired
              name="days"
              label={getText('daysLabel')}
              items={DAYS}
              itemToString={(n) => getText(DAY_3_LETTER_TEXT_IDS[n] ?? 'monday3')}
            />
          )}
          {repeatInterval !== 'hourly' && (
            <MultiSelector
              form={form}
              isRequired
              name="hours"
              label={getText('hoursLabel')}
              items={HOURS}
              columns={12}
            />
          )}
          <Input
            required
            name="minute"
            label={getText('minuteLabel')}
            type="number"
            min={0}
            max={59}
          />
          <Selector
            form={form}
            isRequired
            name="parallelMode"
            label={getText('parallelModeLabel')}
            items={PROJECT_PARALLEL_MODES}
            itemToString={(interval) => getText(PARALLEL_MODE_TO_TEXT_ID[interval])}
          />

          <Form.FormError />
          <ButtonGroup>
            <Form.Submit />
            <Button variant="cancel" onPress={close}>
              {getText('cancel')}
            </Button>
          </ButtonGroup>
        </Form>
      )}
    </Dialog>
  )
}
