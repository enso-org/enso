/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import {
  ZonedDateTime,
  endOfMonth,
  getDayOfWeek,
  getLocalTimeZone,
  now,
} from '@internationalized/date'
import { useMutation } from '@tanstack/react-query'

import type Backend from '#/services/Backend'
import type {
  ProjectExecutionInfo,
  ProjectExecutionRepeatInfo,
  ProjectExecutionRepeatType,
  ProjectId,
} from '#/services/Backend'
import {
  PARALLEL_MODE_TO_DESCRIPTION_ID,
  PARALLEL_MODE_TO_TEXT_ID,
  PROJECT_EXECUTION_REPEAT_TYPES,
  PROJECT_PARALLEL_MODES,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'

import {
  Button,
  ButtonGroup,
  DatePicker,
  Dialog,
  DialogDismiss,
  Form,
  FormDropdown,
  Input,
  MultiSelector,
  Selector,
  Text,
} from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useGetOrdinal } from '#/hooks/ordinalHooks'
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import {
  firstProjectExecutionOnOrAfter,
  nextProjectExecutionDate,
} from 'enso-common/src/services/Backend/projectExecution'
import {
  DAY_3_LETTER_TEXT_IDS,
  DAY_TEXT_IDS,
  MONTH_3_LETTER_TEXT_IDS,
  toRfc3339,
} from 'enso-common/src/utilities/data/dateTime'
import { useEffect, useRef } from 'react'

const MAX_DURATION_DEFAULT_MINUTES = 60
const MAX_DURATION_MINIMUM_MINUTES = 1
const MAX_DURATION_MAXIMUM_MINUTES = 180
const REPEAT_TIMES_COUNT = 5
const DAYS_PER_WEEK = 7
const HOURS_PER_DAY = 24
const MONTHS_PER_YEAR = 12

const DAYS = [...Array(DAYS_PER_WEEK).keys()] as const
const MONTHS = [...Array(MONTHS_PER_YEAR).keys()] as const

/** Create the form schema for this page. */
function createUpsertExecutionSchema(timeZone: string | undefined) {
  return z
    .object({
      projectId: z.string().refine((x: unknown): x is ProjectId => true),
      repeatType: z.enum(PROJECT_EXECUTION_REPEAT_TYPES),
      days: z
        .number()
        .int()
        .min(0)
        .max(DAYS_PER_WEEK - 1)
        .array()
        .min(1)
        .transform((arr) => arr.sort((a, b) => a - b))
        .readonly(),
      months: z
        .number()
        .int()
        .min(0)
        .max(MONTHS_PER_YEAR - 1)
        .array()
        .min(1)
        .transform((arr) => arr.sort((a, b) => a - b))
        .readonly(),
      startHour: z
        .number()
        .int()
        .min(0)
        .max(HOURS_PER_DAY - 1),
      endHour: z
        .number()
        .int()
        .min(0)
        .max(HOURS_PER_DAY - 1),
      startDate: z.instanceof(ZonedDateTime).or(z.null()).optional(),
      maxDurationMinutes: z
        .number()
        .int()
        .min(MAX_DURATION_MINIMUM_MINUTES)
        .max(MAX_DURATION_MAXIMUM_MINUTES),
      parallelMode: z.enum(PROJECT_PARALLEL_MODES),
    })
    .transform(
      ({
        projectId,
        startDate = null,
        repeatType,
        maxDurationMinutes,
        parallelMode,
        days,
        months,
        startHour,
        endHour,
      }): ProjectExecutionInfo => {
        timeZone ??= getLocalTimeZone()
        startDate ??= now(timeZone)
        const startDateTime = toRfc3339(startDate.toDate())
        const repeat = ((): ProjectExecutionRepeatInfo => {
          switch (repeatType) {
            case 'none': {
              return {
                type: repeatType,
              }
            }
            case 'hourly': {
              return {
                type: repeatType,
                startHour: startHour,
                endHour: endHour,
              }
            }
            case 'daily': {
              return {
                type: repeatType,
                daysOfWeek: days,
              }
            }
            case 'monthly-date': {
              return {
                type: repeatType,
                date: startDate.day,
                months,
              }
            }
            case 'monthly-weekday': {
              return {
                type: repeatType,
                dayOfWeek: getDayOfWeek(startDate, 'en-US'),
                weekNumber: Math.floor(startDate.day / DAYS_PER_WEEK) + 1,
                months,
              }
            }
            case 'monthly-last-weekday': {
              return {
                type: repeatType,
                dayOfWeek: getDayOfWeek(startDate, 'en-US'),
                months,
              }
            }
          }
        })()
        return {
          projectId,
          timeZone: timeZone,
          repeat,
          maxDurationMinutes,
          parallelMode,
          startDate: startDateTime,
        }
      },
    )
}

/** Props for a {@link NewProjectExecutionModal}. */
export interface NewProjectExecutionModalProps {
  readonly backend: Backend
  readonly item: ProjectAsset
  readonly defaultOpen?: boolean
  readonly defaultDate?: ZonedDateTime
}

/** A modal for confirming the deletion of an asset. */
export function NewProjectExecutionModal(props: NewProjectExecutionModalProps) {
  const { defaultOpen } = props
  const { getText } = useText()

  return (
    <Dialog title={getText('newProjectExecution')} {...(defaultOpen != null && { defaultOpen })}>
      <NewProjectExecutionForm {...props} />
    </Dialog>
  )
}

/** Props for a {@link NewProjectExecutionForm}. */
export interface NewProjectExecutionFormProps extends NewProjectExecutionModalProps {
  readonly onChange?: (value: ProjectExecutionInfo) => void
  readonly onCancel?: () => void
}

/** A modal for confirming the deletion of an asset. */
export function NewProjectExecutionForm(props: NewProjectExecutionFormProps) {
  const { backend, item, defaultDate, onChange, onCancel } = props
  const { getText } = useText()
  const [preferredTimeZone] = useLocalStorageState('preferredTimeZone')
  const getOrdinal = useGetOrdinal()
  const timeZone = preferredTimeZone ?? getLocalTimeZone()
  const enableAdvancedProjectExecutionOptions = useFeatureFlag(
    'enableAdvancedProjectExecutionOptions',
  )
  const valueJson = useRef('')

  const nowZonedDateTime = now(timeZone)
  const minFirstOccurrence = nowZonedDateTime
  const form = Form.useForm({
    method: 'dialog',
    schema: createUpsertExecutionSchema(preferredTimeZone),
    defaultValues: {
      projectId: item.id,
      repeatType: 'daily',
      parallelMode: 'restart',
      startDate: defaultDate ?? minFirstOccurrence,
      maxDurationMinutes: MAX_DURATION_DEFAULT_MINUTES,
      // Use `en-US` locale because it matches JavaScript conventions.
      days: [getDayOfWeek(minFirstOccurrence, 'en-US')],
      months: [minFirstOccurrence.month - 1],
      startHour: 0,
      endHour: HOURS_PER_DAY - 1,
    },
    onSubmit: async (values) => {
      await createProjectExecution([values, item.title])
    },
  })
  const repeatType = form.watch('repeatType', 'daily')
  const parallelMode = form.watch('parallelMode', 'restart')
  const date = form.watch('startDate', nowZonedDateTime) ?? nowZonedDateTime
  // Reactively watch for `days` and `months` so that repeat dates are kept up to date.
  form.watch('days')
  form.watch('months')
  const daysToEndOfMonth = endOfMonth(date).day - date.day
  const validRepeatTypes =
    daysToEndOfMonth >= DAYS_PER_WEEK ?
      PROJECT_EXECUTION_REPEAT_TYPES.filter((type) => type !== 'monthly-last-weekday')
    : PROJECT_EXECUTION_REPEAT_TYPES

  useEffect(() => {
    if (onChange) {
      const parsed = form.schema.safeParse(form.getValues())
      if (parsed.success) {
        const newJson = JSON.stringify(parsed)
        if (newJson !== valueJson.current) {
          onChange(parsed.data)
          valueJson.current = newJson
        }
      }
    }
  })

  const createProjectExecution = useMutation(
    backendMutationOptions(backend, 'createProjectExecution'),
  ).mutateAsync

  const repeatTimes = (() => {
    const parsed = form.schema.safeParse(form.getValues())
    const projectExecution = parsed.data
    if (!projectExecution) {
      return []
    }
    let nextDate = firstProjectExecutionOnOrAfter(projectExecution, date.toDate())
    const dates = [nextDate]
    while (dates.length < REPEAT_TIMES_COUNT) {
      nextDate = nextProjectExecutionDate(projectExecution, nextDate)
      dates.push(nextDate)
    }
    return dates
  })()

  const repeatText = useEventCallback((otherRepeatType: ProjectExecutionRepeatType) => {
    // Use `en-US` locale because it matches JavaScript conventions.
    const dayOfWeekNumber = getDayOfWeek(date, 'en-US')
    const dayOfWeek = getText(DAY_TEXT_IDS[dayOfWeekNumber] ?? 'monday')
    switch (otherRepeatType) {
      case 'none': {
        return getText('doesNotRepeat')
      }
      case 'hourly': {
        return getText('hourly')
      }
      case 'daily': {
        return getText('daily')
      }
      case 'monthly-date': {
        return getText('xthDayOfMonth', getOrdinal(date.day))
      }
      case 'monthly-weekday': {
        return getText(
          'xthXDayOfMonth',
          getOrdinal(Math.floor(date.day / DAYS_PER_WEEK) + 1),
          dayOfWeek,
        )
      }
      case 'monthly-last-weekday': {
        return getText('lastXDayOfMonth', dayOfWeek)
      }
    }
  })

  return (
    <Form form={form} className="w-full">
      <div className="flex flex-col">
        <DatePicker
          form={form}
          isRequired
          noCalendarHeader
          name="startDate"
          granularity="minute"
          label={getText('firstOccurrenceLabel')}
          minValue={minFirstOccurrence}
        />
        <Text>
          {getText(
            'repeatsAtX',
            (repeatType === 'hourly' ?
              // eslint-disable-next-line @typescript-eslint/unbound-method
              repeatTimes.map(Intl.DateTimeFormat(undefined, { timeStyle: 'short' }).format)
              // eslint-disable-next-line @typescript-eslint/unbound-method
            : repeatTimes.map(Intl.DateTimeFormat(undefined, { dateStyle: 'short' }).format)
            ).join(', '),
          )}
        </Text>
      </div>
      <FormDropdown
        form={form}
        isRequired
        name="repeatType"
        label={getText('repeatIntervalLabel')}
        items={validRepeatTypes}
      >
        {({ item: otherItem }) => repeatText(otherItem)}
      </FormDropdown>
      {repeatType === 'hourly' && (
        <Input
          form={form}
          isRequired
          name="startHour"
          label={getText('startHourLabel')}
          type="number"
          min={0}
          max={HOURS_PER_DAY - 1}
        />
      )}
      {repeatType === 'hourly' && (
        <Input
          form={form}
          isRequired
          name="endHour"
          label={getText('endHourLabel')}
          type="number"
          min={0}
          max={HOURS_PER_DAY - 1}
        />
      )}
      {repeatType === 'daily' && (
        <MultiSelector form={form} isRequired name="days" label={getText('daysLabel')} items={DAYS}>
          {(n) => getText(DAY_3_LETTER_TEXT_IDS[n] ?? 'monday3')}
        </MultiSelector>
      )}
      {(repeatType === 'monthly-date' || repeatType === 'monthly-weekday') && (
        <MultiSelector
          form={form}
          isRequired
          name="months"
          label={getText('monthsLabel')}
          items={MONTHS}
          columns={6}
        >
          {(n) => getText(MONTH_3_LETTER_TEXT_IDS[n] ?? 'january3')}
        </MultiSelector>
      )}
      {enableAdvancedProjectExecutionOptions && (
        <details className="w-full">
          <summary className="cursor-pointer">{getText('advancedOptions')}</summary>
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
          <Input
            form={form}
            name="maxDurationMinutes"
            type="number"
            defaultValue={MAX_DURATION_DEFAULT_MINUTES}
            min={MAX_DURATION_MINIMUM_MINUTES}
            max={MAX_DURATION_MAXIMUM_MINUTES}
            label={getText('maxDurationMinutesLabel')}
          />
        </details>
      )}

      <ButtonGroup>
        <Form.Submit />

        {onCancel ?
          <Button variant="outline" onPress={onCancel}>
            {getText('cancel')}
          </Button>
        : <DialogDismiss />}
      </ButtonGroup>

      <Form.FormError />
    </Form>
  )
}
