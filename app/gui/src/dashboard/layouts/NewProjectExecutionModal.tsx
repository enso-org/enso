/** @file Modal for confirming delete of any type of asset. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { ComboBox } from '#/components/Inputs/ComboBox'
import { DatePicker } from '#/components/Inputs/DatePicker'
import { FormDropdown } from '#/components/Inputs/Dropdown'
import { Input } from '#/components/Inputs/Input'
import { MultiSelector } from '#/components/Inputs/MultiSelector'
import { Selector } from '#/components/Inputs/Selector'
import { Text } from '#/components/Text'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useEventCallback } from '#/hooks/eventCallbackHooks'
import { useLocalStorageState } from '#/hooks/localStoreState'
import { useGetOrdinal } from '#/hooks/ordinalHooks'
import { useSyncRef } from '#/hooks/syncRefHooks'
import { useMutationCallback } from '#/utilities/tanstackQuery'
import { useText } from '$/providers/react'
import { useFeatureFlag } from '$/providers/react/featureFlags'
import { endOfMonth, getLocalTimeZone, now, toZoned, ZonedDateTime } from '@internationalized/date'
import type {
  Backend,
  ProjectExecutionInfo,
  ProjectExecutionRepeatInfo,
  ProjectId,
} from 'enso-common/src/services/Backend'
import {
  PARALLEL_MODE_TO_DESCRIPTION_ID,
  PARALLEL_MODE_TO_TEXT_ID,
  PROJECT_EXECUTION_REPEAT_TYPES,
  PROJECT_PARALLEL_MODES,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'
import {
  firstProjectExecutionOnOrAfter,
  nextProjectExecutionDate,
} from 'enso-common/src/services/Backend/projectExecution'
import {
  DAY_3_LETTER_TEXT_IDS,
  DAY_TEXT_IDS,
  DAYS,
  DAYS_PER_WEEK,
  getDay,
  getDescriptionForTimeZone,
  getTimeZoneFromDescription,
  getTimeZoneOffsetStringWithGMT,
  getWeekOfMonth,
  IanaTimeZone,
  MONTH_3_LETTER_TEXT_IDS,
  MONTHS,
  MONTHS_PER_YEAR,
  toRfc3339,
  WHITELISTED_TIME_ZONE_DESCRIPTIONS,
  zonedDateTimeToReadableIsoString,
} from 'enso-common/src/utilities/data/dateTime'
import { useEffect, useRef, useState } from 'react'
import * as z from 'zod'

// This is a SAFE upcast.
// eslint-disable-next-line no-restricted-syntax
const DISABLE_LAST_WEEKDAY_REPEAT_TYPE = true as boolean
const MAX_DURATION_DEFAULT_MINUTES = 60
const MAX_DURATION_MINIMUM_MINUTES = 1
const MAX_DURATION_MAXIMUM_MINUTES = 180
const REPEAT_TIMES_COUNT = 3

/** The form schema for this page. */
const UPSERT_EXECUTION_SCHEMA = z
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
      timeZone: description,
    }): ProjectExecutionInfo => {
      const timeZone = getTimeZoneFromDescription(description)
      startDate ??= now(timeZone)
      const startDateTime = toRfc3339(new Date(startDate.toAbsoluteString()))
      const repeat = ((): ProjectExecutionRepeatInfo => {
        switch (repeatType) {
          case 'none': {
            return {
              type: repeatType,
            }
          }
          case 'daily': {
            return {
              type: repeatType,
            }
          }
          case 'weekly': {
            return {
              type: repeatType,
              daysOfWeek: days,
            }
          }
          case 'monthlyDate': {
            return {
              type: repeatType,
              date: startDate.day,
              months,
            }
          }
          case 'monthlyWeekday': {
            return {
              type: repeatType,
              dayOfWeek: getDay(startDate),
              weekNumber: getWeekOfMonth(startDate.day),
              months,
            }
          }
          case 'monthlyLastWeekday': {
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
  const timeZone = IanaTimeZone(preferredTimeZone ?? getLocalTimeZone())
  const timeZoneDescription = getDescriptionForTimeZone(timeZone)
  const enableAdvancedProjectExecutionOptions = useFeatureFlag(
    'enableAdvancedProjectExecutionOptions',
  )
  const valueJson = useRef('')

  // Only initialize `minFirstOccurrence` once.
  // Initialize to the start of today.
  const [minFirstOccurrence] = useState(() =>
    now(timeZone).set({ hour: 0, minute: 0, second: 0, millisecond: 0 }),
  )
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
      timeZone: timeZoneDescription,
    },
    onSubmit: async (values) => {
      await createProjectExecution([values, item.title])
    },
  })
  const repeatType = form.watch('repeatType', 'daily')
  const parallelMode = form.watch('parallelMode', 'restart')
  const date = form.watch('startDate', defaultStartDate) ?? defaultStartDate
  // `timeZone` may be `null`.
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-condition
  const formTimeZoneDescription = form.watch('timeZone', timeZoneDescription) ?? timeZoneDescription
  const formTimeZone = getTimeZoneFromDescription(formTimeZoneDescription)
  // Reactively watch for `days` and `months` so that repeat dates are kept up to date.
  form.watch('days')
  form.watch('months')
  const daysToEndOfMonth = endOfMonth(date).day - date.day
  const validRepeatTypes =
    DISABLE_LAST_WEEKDAY_REPEAT_TYPE || daysToEndOfMonth >= DAYS_PER_WEEK ?
      PROJECT_EXECUTION_REPEAT_TYPES.filter((type) => type !== 'monthlyLastWeekday')
    : PROJECT_EXECUTION_REPEAT_TYPES

  const changeTimezoneDeps = useSyncRef({ date, form })
  useEffect(() => {
    const deps = changeTimezoneDeps.current
    deps.form.setValue('startDate', toZoned(deps.date, formTimeZone))
  }, [formTimeZone, changeTimezoneDeps])

  useEffect(() => {
    if (!onChange) {
      return
    }
    const parsed = form.schema.safeParse(form.getValues())
    if (parsed.success) {
      const newJson = JSON.stringify(parsed)
      if (newJson !== valueJson.current) {
        onChange(parsed.data)
        valueJson.current = newJson
      }
    }
  })

  const createProjectExecution = useMutationCallback(
    backendMutationOptions(backend, 'createProjectExecution'),
  )

  const repeatTimes = (() => {
    const parsed = form.schema.safeParse(form.getValues())
    const projectExecution = parsed.data
    if (!projectExecution) {
      return []
    }
    let nextDate: ZonedDateTime | null = firstProjectExecutionOnOrAfter(projectExecution, date)
    const dates = date.compare(nextDate) !== 0 ? [nextDate] : []
    nextDate = nextProjectExecutionDate(projectExecution, nextDate)
    while (nextDate && dates.length < REPEAT_TIMES_COUNT) {
      dates.push(nextDate)
      nextDate = nextProjectExecutionDate(projectExecution, nextDate)
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
      case 'monthlyDate': {
        return getText('monthlyXthDay', getOrdinal(date.day))
      }
      case 'monthlyWeekday': {
        return getText('monthlyXthXDay', getOrdinal(getWeekOfMonth(date.day)), dayOfWeek)
      }
      case 'monthlyLastWeekday': {
        return getText('monthlyLastXDay', dayOfWeek)
      }
    }
  })

  return (
    <Form form={form} className="w-full">
      <ComboBox
        form={form}
        isRequired
        name="timeZone"
        label={getText('timeZoneLabel')}
        items={WHITELISTED_TIME_ZONE_DESCRIPTIONS}
        addonStart={
          <Text className="w-20">
            {getTimeZoneOffsetStringWithGMT(toZoned(date, formTimeZone))}
          </Text>
        }
        toTextValue={(otherTimeZone) => otherTimeZone}
        className="w-full"
      >
        {(description) => {
          const otherTimeZone = getTimeZoneFromDescription(description)
          const timezoneOffsetString = getTimeZoneOffsetStringWithGMT(toZoned(date, otherTimeZone))
          return `${timezoneOffsetString} ${description}`
        }}
      </ComboBox>
      <DatePicker
        form={form}
        isRequired
        noCalendarHeader
        name="startDate"
        hideTimeZone
        label={getText('firstOccurrenceLabel')}
        minValue={minFirstOccurrence}
        className="w-full"
      />
      <FormDropdown
        form={form}
        isRequired
        name="repeatType"
        label={getText('repeatIntervalLabel')}
        items={validRepeatTypes}
        size="medium"
        className="w-full"
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
      {(repeatType === 'monthlyDate' || repeatType === 'monthlyWeekday') && (
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
        <Text>{getText('ellipsis')}</Text>
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

      <Button.Group>
        <Form.Submit />

        {onCancel ?
          <Button variant="outline" onPress={onCancel}>
            {getText('cancel')}
          </Button>
        : <Dialog.Close variant="outline">{getText('cancel')}</Dialog.Close>}
      </Button.Group>

      <Form.FormError />
    </Form>
  )
}
