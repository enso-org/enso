/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import type Backend from '#/services/Backend'
import {
  PROJECT_PARALLEL_MODES,
  PROJECT_REPEAT_INTERVALS,
  type ProjectAsset,
  type ProjectParallelMode,
  type ProjectRepeatInterval,
} from 'enso-common/src/services/Backend'
import type { TextId } from 'enso-common/src/text'

import {
  Button,
  ButtonGroup,
  Dialog,
  Form,
  Input,
  MultiSelector,
  Selector,
} from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import { useMutation } from '@tanstack/react-query'
import { DAY_3_LETTER_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'

const REPEAT_INTERVAL_TO_TEXT = {
  hourly: 'hourlyRepeatInterval',
  daily: 'dailyRepeatInterval',
  weekly: 'weeklyRepeatInterval',
  monthly: 'monthlyRepeatInterval',
} satisfies {
  [K in ProjectRepeatInterval]: TextId & `${K}RepeatInterval`
}

const PARALLEL_MODE_TO_TEXT = {
  ignore: 'ignoreParallelMode',
  restart: 'restartParallelMode',
  parallel: 'parallelParallelMode',
} satisfies {
  [K in ProjectParallelMode]: TextId & `${K}ParallelMode`
}

/* eslint-disable @typescript-eslint/no-magic-numbers */
const DATES = [...Array(31).keys()]
const DAYS = [...Array(7).keys()]
const HOURS = [...Array(24).keys()]

/** Create the form schema for this page. */
function createUpsertExecutionSchema() {
  return z
    .object({
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
    .superRefine((object, context) => {
      const unrecognizedKeys: string[] = []
      switch (object.repeatInterval) {
        case 'hourly':
          if ('hours' in object) {
            unrecognizedKeys.push('hours')
          }
        // fallthrough
        case 'daily':
          if ('days' in object) {
            unrecognizedKeys.push('days')
          }
        // fallthrough
        case 'weekly':
          if ('dates' in object) {
            unrecognizedKeys.push('dates')
          }
          break
        case 'monthly': {
          if ('days' in object) {
            unrecognizedKeys.push('days')
          }
          break
        }
      }
      if (unrecognizedKeys.length > 0) {
        context.addIssue({ code: 'unrecognized_keys', keys: unrecognizedKeys })
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

  const form = Form.useForm({ schema: createUpsertExecutionSchema() })

  const repeatInterval = form.watch('repeatInterval', 'weekly')

  const createProjectExecution = useMutation({
    mutationKey: [backend.type, 'createProjectExecution'],
    mutationFn: async (parameters: Parameters<(typeof backend)['createProjectExecution']>) => {
      await backend.createProjectExecution(...parameters)
    },
    meta: { invalidates: [[backend.type, 'listProjectExecutions']], awaitInvalidates: true },
  }).mutateAsync

  return (
    <Dialog title={getText('newProjectExecution')}>
      {({ close }) => (
        <Form
          form={form}
          method="dialog"
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
            defaultValue="weekly"
            items={PROJECT_REPEAT_INTERVALS}
            itemToString={(interval) => getText(REPEAT_INTERVAL_TO_TEXT[interval])}
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
            readOnly
            isRequired
            name="minute"
            label={getText('minuteLabel')}
            type="number"
            min={0}
            max={59}
            defaultValue={0}
          />
          <Selector
            form={form}
            isRequired
            name="parallelMode"
            label={getText('parallelModeLabel')}
            defaultValue="restart"
            items={PROJECT_PARALLEL_MODES}
            itemToString={(interval) => getText(PARALLEL_MODE_TO_TEXT[interval])}
          />
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
