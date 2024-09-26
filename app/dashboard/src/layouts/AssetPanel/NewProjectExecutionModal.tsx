/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import {
  ZonedDateTime,
  getDayOfWeek,
  getLocalTimeZone,
  now,
  toCalendarDate,
  toTimeZone,
} from '@internationalized/date'
import { useMutation } from '@tanstack/react-query'

import type Backend from '#/services/Backend'
import {
  PARALLEL_MODE_TO_DESCRIPTION_ID,
  PARALLEL_MODE_TO_TEXT_ID,
  PROJECT_PARALLEL_MODES,
  PROJECT_REPEAT_INTERVALS,
  REPEAT_INTERVAL_TO_TEXT_ID,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'

import {
  ButtonGroup,
  DatePicker,
  Dialog,
  Form,
  Input,
  MultiSelector,
  Selector,
  Switch,
  Text,
} from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useText, type GetText } from '#/providers/TextProvider'
import { DAY_3_LETTER_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'

const MAX_DURATION_DEFAULT_MINUTES = 60
const MAX_DURATION_MINIMUM_MINUTES = 1
const MAX_DURATION_MAXIMUM_MINUTES = 180
const REPEAT_TIMES_COUNT = 5

/* eslint-disable @typescript-eslint/no-magic-numbers */
const DATES: readonly number[] = [...Array(31).keys()]
const DAYS: readonly number[] = [...Array(7).keys()]
const HOURS: readonly number[] = [...Array(24).keys()]

/** Create the form schema for this page. */
function createUpsertExecutionSchema(getText: GetText) {
  return z
    .object({
      multiSelect: z.boolean(),
      repeatInterval: z.enum(PROJECT_REPEAT_INTERVALS),
      dates: z
        .number()
        .int()
        .min(0)
        .max(30)
        .array()
        .transform((arr) => arr.sort((a, b) => a - b))
        .readonly(),
      days: z
        .number()
        .int()
        .min(0)
        .max(6)
        .array()
        .transform((arr) => arr.sort((a, b) => a - b))
        .readonly(),
      hours: z
        .number()
        .int()
        .min(0)
        .max(23)
        .array()
        .transform((arr) => arr.sort((a, b) => a - b))
        .readonly(),
      minute: z.number(),
      date: z.instanceof(ZonedDateTime, { message: getText('pleaseSelectATime') }),
      maxDurationMinutes: z
        .number()
        .int()
        .min(MAX_DURATION_MINIMUM_MINUTES)
        .max(MAX_DURATION_MAXIMUM_MINUTES),
      parallelMode: z.enum(PROJECT_PARALLEL_MODES),
    })
    .transform(
      ({
        date,
        repeatInterval,
        multiSelect,
        maxDurationMinutes,
        parallelMode,
        dates,
        days,
        hours,
        minute,
      }) => {
        const utcDate = toTimeZone(date, 'UTC')
        if (multiSelect) {
          return {
            repeatInterval,
            maxDurationMinutes,
            parallelMode,
            time: {
              ...(repeatInterval === 'monthly' && { dates: [utcDate.day] }),
              ...(repeatInterval === 'weekly' && { days: [getDayOfWeek(utcDate, 'en-US')] }),
              ...(repeatInterval !== 'hourly' && { hours: [utcDate.hour] }),
              minute: utcDate.minute,
            },
          }
        } else {
          return {
            repeatInterval,
            maxDurationMinutes,
            parallelMode,
            time: {
              ...(repeatInterval === 'monthly' && { dates }),
              ...(repeatInterval === 'weekly' && { days }),
              ...(repeatInterval !== 'hourly' && { hours }),
              minute,
            },
          }
        }
      },
    )
}
/* eslint-enable @typescript-eslint/no-magic-numbers */

// ================================
// === NewProjectExecutionModal ===
// ================================

/** Props for a {@link NewProjectExecutionModal}. */
export interface NewProjectExecutionModalProps {
  readonly backend: Backend
  readonly item: ProjectAsset
  readonly defaultOpen?: boolean
}

/** A modal for confirming the deletion of an asset. */
export function NewProjectExecutionModal(props: NewProjectExecutionModalProps) {
  const { defaultOpen } = props
  const { getText } = useText()

  return (
    <Dialog title={getText('newProjectExecution')} {...(defaultOpen != null && { defaultOpen })}>
      <NewProjectExecutionModalInner {...props} />
    </Dialog>
  )
}

/** A modal for confirming the deletion of an asset. */
function NewProjectExecutionModalInner(props: NewProjectExecutionModalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const nowZonedDateTime = now(getLocalTimeZone())
  const minFirstOccurrence = nowZonedDateTime
  const form = Form.useForm({
    method: 'dialog',
    schema: createUpsertExecutionSchema(getText),
    defaultValues: {
      multiSelect: false,
      repeatInterval: 'weekly',
      parallelMode: 'restart',
      date: minFirstOccurrence,
      maxDurationMinutes: MAX_DURATION_DEFAULT_MINUTES,
      dates: [],
      days: [],
      hours: [],
      minute: 0,
    },
    onSubmit: async (values) => {
      await createProjectExecution([{ projectId: item.id, ...values }, item.title])
    },
  })
  const repeatInterval = form.watch('repeatInterval', 'weekly')
  const parallelMode = form.watch('parallelMode', 'restart')
  const date = form.watch('date', nowZonedDateTime)
  const multiSelect = form.watch('multiSelect', false)

  const createProjectExecution = useMutation(
    backendMutationOptions(backend, 'createProjectExecution'),
  ).mutateAsync

  const maxFirstOccurrence = (() => {
    switch (repeatInterval) {
      case 'hourly': {
        return minFirstOccurrence.add({ hours: 1 })
      }
      case 'daily': {
        return minFirstOccurrence.add({ days: 1 })
      }
      case 'weekly': {
        return minFirstOccurrence.add({ weeks: 1 })
      }
      case 'monthly': {
        return minFirstOccurrence.add({ months: 1 })
      }
    }
  })()

  const repeatTimes = Array.from({ length: REPEAT_TIMES_COUNT }, (_, i) => {
    switch (repeatInterval) {
      case 'hourly': {
        return date.add({ hours: i })
      }
      case 'daily': {
        return date.add({ days: i })
      }
      case 'weekly': {
        return date.add({ weeks: i })
      }
      case 'monthly': {
        return date.add({ months: i })
      }
    }
  })

  return (
    <Form form={form} className="w-full">
      {!multiSelect && (
        <>
          <Selector
            form={form}
            isRequired
            name="repeatInterval"
            label={getText('repeatIntervalLabel')}
            items={PROJECT_REPEAT_INTERVALS}
          >
            {(interval) => getText(REPEAT_INTERVAL_TO_TEXT_ID[interval])}
          </Selector>
          <div className="flex w-full flex-col">
            <Selector
              form={form}
              isRequired
              name="parallelMode"
              label={getText('parallelModeLabel')}
              items={PROJECT_PARALLEL_MODES}
            >
              {(interval) => getText(PARALLEL_MODE_TO_TEXT_ID[interval])}
            </Selector>
            <Text>{getText(PARALLEL_MODE_TO_DESCRIPTION_ID[parallelMode])}</Text>
          </div>
          {repeatInterval !== 'hourly' && (
            <div className="flex flex-col">
              <DatePicker
                form={form}
                isRequired
                name="date"
                label={getText('firstOccurrenceLabel')}
                noCalendarHeader
                minValue={minFirstOccurrence}
                maxValue={maxFirstOccurrence}
              />
              <Text>
                {getText(
                  'repeatsAtX',
                  repeatTimes
                    .map((time) =>
                      toCalendarDate(time).toDate(getLocalTimeZone()).toLocaleDateString(),
                    )
                    .join(', '),
                )}
              </Text>
            </div>
          )}
          <Input
            form={form}
            name="maxDurationMinutes"
            type="number"
            defaultValue={MAX_DURATION_DEFAULT_MINUTES}
            min={MAX_DURATION_MINIMUM_MINUTES}
            max={MAX_DURATION_MAXIMUM_MINUTES}
            label={getText('maxDurationMinutesLabel')}
          />
        </>
      )}
      {multiSelect && (
        <>
          <Selector
            form={form}
            name="repeatInterval"
            label={getText('repeatIntervalLabel')}
            items={PROJECT_REPEAT_INTERVALS}
          >
            {(interval) => getText(REPEAT_INTERVAL_TO_TEXT_ID[interval])}
          </Selector>
          {repeatInterval === 'monthly' && (
            <MultiSelector
              form={form}
              isRequired
              name="dates"
              label={getText('datesLabel')}
              items={DATES}
              columns={10}
            >
              {(n) => String(n + 1)}
            </MultiSelector>
          )}
          {repeatInterval === 'weekly' && (
            <MultiSelector
              form={form}
              isRequired
              name="days"
              label={getText('daysLabel')}
              items={DAYS}
            >
              {(n) => getText(DAY_3_LETTER_TEXT_IDS[n] ?? 'monday3')}
            </MultiSelector>
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
            form={form}
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
          >
            {(interval) => getText(PARALLEL_MODE_TO_TEXT_ID[interval])}
          </Selector>
        </>
      )}
      <Switch form={form} label={getText('advancedModeLabel')} name="multiSelect" />

      <Form.FormError />
      <ButtonGroup>
        <Form.Submit />
        <Form.Submit formnovalidate variant="outline">
          {getText('cancel')}
        </Form.Submit>
      </ButtonGroup>
    </Form>
  )
}
