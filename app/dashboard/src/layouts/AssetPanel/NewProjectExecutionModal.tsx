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
  type ProjectScheduleTime,
} from 'enso-common/src/services/Backend'

import {
  Button,
  ButtonGroup,
  DatePicker,
  Dialog,
  Form,
  Input,
  Selector,
  Text,
} from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useText, type GetText } from '#/providers/TextProvider'

const MAX_DURATION_DEFAULT_MINUTES = 60
const MAX_DURATION_MINIMUM_MINUTES = 1
const MAX_DURATION_MAXIMUM_MINUTES = 180
const REPEAT_TIMES_COUNT = 5

/* eslint-disable @typescript-eslint/no-magic-numbers */
/** Create the form schema for this page. */
function createUpsertExecutionSchema(getText: GetText) {
  return z
    .object({
      repeatInterval: z.enum(PROJECT_REPEAT_INTERVALS),
      date: z.instanceof(ZonedDateTime, { message: getText('pleaseSelectATime') }),
      maxDurationMinutes: z
        .number()
        .int()
        .min(MAX_DURATION_MINIMUM_MINUTES)
        .max(MAX_DURATION_MAXIMUM_MINUTES),
      parallelMode: z.enum(PROJECT_PARALLEL_MODES),
    })
    .transform(({ date, repeatInterval, ...rest }) => {
      const utcDate = toTimeZone(date, 'UTC')
      return {
        repeatInterval,
        ...(repeatInterval === 'monthly' && { date: utcDate.day }),
        ...(repeatInterval === 'weekly' && { day: getDayOfWeek(utcDate, 'en-US') }),
        ...(repeatInterval !== 'hourly' && { hour: utcDate.hour }),
        minute: utcDate.minute,
        ...rest,
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
  readonly defaultOpen?: boolean
}

/** A modal for confirming the deletion of an asset. */
export default function NewProjectExecutionModal(props: NewProjectExecutionModalProps) {
  const { backend, item, defaultOpen } = props
  const { getText } = useText()

  const nowZonedDateTime = now(getLocalTimeZone())
  const minFirstOccurrence = nowZonedDateTime
  const form = Form.useForm({
    schema: createUpsertExecutionSchema(getText),
    defaultValues: {
      repeatInterval: 'weekly',
      parallelMode: 'restart',
      date: minFirstOccurrence,
      maxDurationMinutes: MAX_DURATION_DEFAULT_MINUTES,
    },
    onSubmit: async (values) => {
      const { date: newDate, ...rest } = values
      const time: ProjectScheduleTime = {
        minute: newDate.minute,
        hours: [newDate.hour],
        days: [],
        dates: [newDate.day],
      }
      await createProjectExecution([{ projectId: item.id, time, ...rest }, item.title])
    },
  })
  const repeatInterval = form.watch('repeatInterval', 'weekly')
  const parallelMode = form.watch('parallelMode', 'restart')
  const date = form.watch('date', nowZonedDateTime)

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
    <Dialog title={getText('newProjectExecution')} {...(defaultOpen != null && { defaultOpen })}>
      {({ close }) => (
        <Form form={form} method="dialog" className="w-full">
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

          <Form.FormError />
          <ButtonGroup>
            <Form.Submit />
            <Button variant="outline" onPress={close}>
              {getText('cancel')}
            </Button>
          </ButtonGroup>
        </Form>
      )}
    </Dialog>
  )
}
