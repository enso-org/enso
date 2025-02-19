/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import {
  endOfMonth,
  getLocalTimeZone,
  now,
  parseZonedDateTime,
  toZoned,
  ZonedDateTime,
} from '@internationalized/date'
import { useMutation } from '@tanstack/react-query'

import type Backend from '#/services/Backend'
import type {
  ProjectExecutionInfo,
  ProjectExecutionRepeatInfo,
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
  ComboBox,
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
  getDay,
  HOUR_MINUTES,
  MINUTE_MS,
  MONTH_3_LETTER_TEXT_IDS,
  toRfc3339,
  WHITELISTED_TIME_ZONE_MAP,
  WHITELISTED_TIME_ZONES,
  zonedDateTimeToReadableIsoString,
} from 'enso-common/src/utilities/data/dateTime'
import { useEffect, useRef } from 'react'

const MAX_DURATION_DEFAULT_MINUTES = 60
const MAX_DURATION_MINIMUM_MINUTES = 1
const MAX_DURATION_MAXIMUM_MINUTES = 180
const REPEAT_TIMES_COUNT = 3
const DAYS_PER_WEEK = 7
const MONTHS_PER_YEAR = 12
const INTERNAL_REPEAT_TYPES = [
  'none',
  'daily',
  'weekly',
  'monthly-date',
  'monthly-weekday',
  'monthly-last-weekday',
] as const

const DAYS = [...Array(DAYS_PER_WEEK).keys()] as const
const MONTHS = [...Array(MONTHS_PER_YEAR).keys()] as const

/** The form schema for this page. */
const UPSERT_EXECUTION_SCHEMA = z
  .object({
    projectId: z.string().refine((x: unknown): x is ProjectId => true),
    repeatType: z.enum([...PROJECT_EXECUTION_REPEAT_TYPES, 'weekly']),
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
    startDate: z.instanceof(ZonedDateTime).or(z.null()).optional(),
    timeZone: z.string(),
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
      timeZone,
    }): ProjectExecutionInfo => {
      startDate ??= now(timeZone)
      const startDateTime = toRfc3339(
        parseZonedDateTime(startDate.toAbsoluteString().replace(/Z$/, `[${timeZone}]`)).toDate(),
      )
      const repeat = ((): ProjectExecutionRepeatInfo => {
        switch (repeatType) {
          case 'none': {
            return {
              type: repeatType,
            }
          }
          case 'daily': {
            return {
              type: 'daily',
              daysOfWeek: DAYS,
            }
          }
          case 'weekly': {
            return {
              type: 'daily',
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
              dayOfWeek: getDay(startDate),
              weekNumber: Math.floor(startDate.day / DAYS_PER_WEEK) + 1,
              months,
            }
          }
          case 'monthly-last-weekday': {
            return {
              type: repeatType,
              dayOfWeek: getDay(startDate),
              months,
            }
          }
        }
      })()
      return {
        projectId,
        timeZone,
        repeat,
        maxDurationMinutes,
        parallelMode,
        startDate: startDateTime,
        endDate: null,
      }
    },
  )

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

  const minFirstOccurrence = now('UTC')
  const defaultStartDate = defaultDate ?? minFirstOccurrence
  const form = Form.useForm({
    method: 'dialog',
    schema: UPSERT_EXECUTION_SCHEMA,
    defaultValues: {
      projectId: item.id,
      repeatType: 'daily',
      parallelMode: 'restart',
      startDate: defaultStartDate,
      maxDurationMinutes: MAX_DURATION_DEFAULT_MINUTES,
      days: DAYS,
      months: MONTHS,
      timeZone,
    },
    onSubmit: async (values) => {
      await createProjectExecution([values, item.title])
    },
  })
  const repeatType = form.watch('repeatType', 'daily')
  const parallelMode = form.watch('parallelMode', 'restart')
  const date = form.watch('startDate', defaultStartDate) ?? defaultStartDate
  const formTimeZone = form.watch('timeZone', timeZone)
  // Reactively watch for `days` and `months` so that repeat dates are kept up to date.
  form.watch('days')
  form.watch('months')
  const daysToEndOfMonth = endOfMonth(date).day - date.day
  const validRepeatTypes =
    daysToEndOfMonth >= DAYS_PER_WEEK ?
      INTERNAL_REPEAT_TYPES.filter((type) => type !== 'monthly-last-weekday')
    : INTERNAL_REPEAT_TYPES

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
    let nextDate = firstProjectExecutionOnOrAfter(projectExecution, toZoned(date, formTimeZone))
    const dates = [nextDate]
    while (dates.length < REPEAT_TIMES_COUNT) {
      nextDate = nextProjectExecutionDate(projectExecution, nextDate)
      dates.push(nextDate)
    }
    return dates
  })()

  const repeatText = useEventCallback((otherRepeatType: typeof repeatType) => {
    // Use `en-US` locale because it matches JavaScript conventions.
    const dayOfWeekNumber = getDay(date)
    const dayOfWeek = getText(DAY_TEXT_IDS[dayOfWeekNumber] ?? 'monday')
    switch (otherRepeatType) {
      case 'none': {
        return getText('doesNotRepeat')
      }
      case 'daily': {
        return getText('daily')
      }
      case 'weekly': {
        return getText('weekly')
      }
      case 'monthly-date': {
        return getText('monthlyXthDay', getOrdinal(date.day))
      }
      case 'monthly-weekday': {
        return getText(
          'monthlyXthXDay',
          getOrdinal(Math.floor(date.day / DAYS_PER_WEEK) + 1),
          dayOfWeek,
        )
      }
      case 'monthly-last-weekday': {
        return getText('monthlyLastXDay', dayOfWeek)
      }
    }
  })

  return (
    <Form form={form} className="w-full">
      <DatePicker
        form={form}
        isRequired
        noCalendarHeader
        name="startDate"
        hideTimeZone
        label={getText('firstOccurrenceLabel')}
        minValue={minFirstOccurrence}
        className="w-60"
      />
      <ComboBox
        form={form}
        isRequired
        name="timeZone"
        label={getText('timeZoneLabel')}
        items={WHITELISTED_TIME_ZONES}
        className="w-60"
      >
        {(otherTimeZone) => {
          const offsetMin = toZoned(date, otherTimeZone).offset / MINUTE_MS
          const offsetNegative = offsetMin < 0
          const absoluteOffsetMin = Math.abs(offsetMin)
          const offsetHours = Math.floor(absoluteOffsetMin / HOUR_MINUTES)
          const offsetMinutes = absoluteOffsetMin % HOUR_MINUTES
          const description =
            WHITELISTED_TIME_ZONE_MAP.get(otherTimeZone)?.timeZone ?? otherTimeZone
          return `(GMT${offsetNegative ? '-' : '+'}${`${offsetHours}`.padStart(2, '0')}:${`${offsetMinutes}`.padStart(2, '0')}) ${description}`
        }}
      </ComboBox>
      <FormDropdown
        form={form}
        isRequired
        name="repeatType"
        label={getText('repeatIntervalLabel')}
        items={validRepeatTypes}
        size="medium"
        className="w-60"
      >
        {({ item: otherItem }) => repeatText(otherItem)}
      </FormDropdown>
      {repeatType === 'weekly' && (
        <MultiSelector
          form={form}
          isRequired
          name="days"
          label={getText('daysLabel')}
          items={DAYS}
          variant="separate-outline"
        >
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
          variant="separate-outline"
        >
          {(n) => getText(MONTH_3_LETTER_TEXT_IDS[n] ?? 'january3')}
        </MultiSelector>
      )}
      <div className={repeatType === 'none' ? 'hidden' : ''}>
        <Text>{getText('repeatsAt')}</Text>
        {repeatTimes.map((dateTime, i) => (
          <Text key={i}>{zonedDateTimeToReadableIsoString(dateTime)}</Text>
        ))}
      </div>
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
