/** @file Displays information describing a specific version of an asset. */
import { useMutation } from '@tanstack/react-query'

import {
  DAY_3_LETTER_TEXT_IDS,
  DAY_TEXT_IDS,
  DAYS_PER_WEEK,
  HOUR_MINUTE,
  HOURS_PER_DAY,
  MINUTE_MS,
  MONTH_3_LETTER_TEXT_IDS,
} from 'enso-common/src/utilities/data/dateTime'

import RepeatIcon from '#/assets/repeat.svg'
import TimeIcon from '#/assets/time.svg'
import { DialogTrigger } from '#/components/aria'
import { Button, ButtonGroup, CloseButton } from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useGetOrdinal } from '#/hooks/ordinalHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { getLocalTimeZone, now } from '@internationalized/date'

const MONTHS_IN_YEAR = 12

const PROJECT_EXECUTION_STYLES = tv({
  base: 'group flex flex-row gap-1 w-full rounded-default items-center odd:bg-primary/5 p-2',
  variants: {
    isEnabled: { false: { time: 'opacity-50', optionContainer: 'opacity-50' } },
  },
  slots: {
    timeContainer: 'flex flex-row items-center gap-2 grow px-2 py-0.5',
    times: 'flex flex-col max-h-[10lh] overflow-auto grow',
    time: '',
    timeButtons: 'opacity-0 group-hover:opacity-100 transition-[opacity]',
    optionContainer: 'flex flex-col grow-0 gap-1',
    maximumDuration: 'cursor-default hover:bg-transparent',
    repeatInterval: 'cursor-default',
    parallelMode: 'cursor-default',
  },
})

/** Props for a {@link ProjectExecution}. */
export interface ProjectExecutionProps {
  /** Defaults to `false`. */
  readonly hideDay?: boolean
  readonly backend: Backend
  readonly item: backendModule.ProjectAsset
  readonly projectExecution: backendModule.ProjectExecution
  /** Defaults to the first date of `projectExecution` if not given. */
  readonly date?: Date
}

/** Displays information describing a specific version of an asset. */
export function ProjectExecution(props: ProjectExecutionProps) {
  const { backend, item, projectExecution, date } = props
  const { getText } = useText()
  const getOrdinal = useGetOrdinal()
  const [timeZone] = useLocalStorageState('preferredTimeZone')
  const { repeat } = projectExecution

  const timeZoneOffsetMs =
    now(timeZone ?? getLocalTimeZone()).offset - now(projectExecution.timeZone).offset
  const timeZoneOffsetMinutesTotal = Math.trunc(timeZoneOffsetMs / MINUTE_MS)
  let timeZoneOffsetHours = Math.floor(timeZoneOffsetMinutesTotal / HOUR_MINUTE)
  const timeZoneOffsetMinutes = timeZoneOffsetMinutesTotal - timeZoneOffsetHours * HOUR_MINUTE

  const startDate = new Date(projectExecution.startDate)
  let minute = startDate.getMinutes()
  minute += timeZoneOffsetMinutes
  while (minute < 0) {
    minute += HOUR_MINUTE
    timeZoneOffsetHours += 1
  }
  while (minute > HOUR_MINUTE) {
    minute -= HOUR_MINUTE
    timeZoneOffsetHours -= 1
  }
  const minuteString = String(minute).padStart(2, '0')
  const startDateHour = (startDate.getHours() + timeZoneOffsetHours + HOURS_PER_DAY) % HOURS_PER_DAY
  const startDateDailyRepeat = getText(
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    startDateHour > 11 ? 'xPm' : 'xAm',
    // eslint-disable-next-line @typescript-eslint/no-magic-numbers
    `${startDateHour % 12 || 12}:${minuteString}`,
  )

  const repeatString = (() => {
    if (date) {
      const hour = (date.getHours() + timeZoneOffsetHours + HOURS_PER_DAY) % HOURS_PER_DAY
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      return getText(hour > 11 ? 'xPm' : 'xAm', `${hour % 12 || 12}:${minuteString}`)
    } else {
      switch (repeat.type) {
        case 'none': {
          return getText('doesNotRepeat')
        }
        case 'hourly': {
          const startHour = (repeat.startHour + timeZoneOffsetHours + HOURS_PER_DAY) % HOURS_PER_DAY
          const endHour = (repeat.endHour + timeZoneOffsetHours + HOURS_PER_DAY) % HOURS_PER_DAY
          return getText(
            'hourlyBetweenX',
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            getText(startHour > 11 ? 'xPm' : 'xAm', `${startHour % 12 || 12}:${minuteString}`),
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            getText(endHour > 11 ? 'xPm' : 'xAm', `${endHour % 12 || 12}:${minuteString}`),
          )
        }
        case 'daily': {
          const dayNames =
            repeat.daysOfWeek.length === DAYS_PER_WEEK ?
              getText('everyDaySuffix')
            : repeat.daysOfWeek
                .map((day) => getText(DAY_3_LETTER_TEXT_IDS[day] ?? 'monday3'))
                .join(', ')
          return `${startDateDailyRepeat} ${dayNames}`
        }
        case 'monthly-date':
        case 'monthly-weekday':
        case 'monthly-last-weekday': {
          const monthNames =
            repeat.months.length === MONTHS_IN_YEAR ?
              getText('everyMonth')
            : repeat.months
                .map((month) => getText(MONTH_3_LETTER_TEXT_IDS[month] ?? 'january3'))
                .join(', ')
          switch (repeat.type) {
            case 'monthly-date': {
              return getText(
                'repeatsTimeXMonthsXDateX',
                startDateDailyRepeat,
                monthNames,
                getOrdinal(repeat.date),
              )
            }
            case 'monthly-weekday': {
              return getText(
                'repeatsTimeXMonthsXDayXWeekX',
                startDateDailyRepeat,
                monthNames,
                getText(DAY_TEXT_IDS[repeat.dayOfWeek] ?? 'monday'),
                getText('xthWeek', getOrdinal(repeat.weekNumber)),
              )
            }
            case 'monthly-last-weekday': {
              return getText(
                'repeatsTimeXMonthsXDayXLastWeek',
                startDateDailyRepeat,
                monthNames,
                getText(DAY_TEXT_IDS[repeat.dayOfWeek] ?? 'monday'),
              )
            }
          }
        }
      }
    }
  })()

  const styles = PROJECT_EXECUTION_STYLES({
    isEnabled: projectExecution.enabled,
  })

  const deleteProjectExecution = useMutation(
    backendMutationOptions(backend, 'deleteProjectExecution'),
  )

  return (
    <div className={styles.base()}>
      <div className={styles.timeContainer()}>
        <div className={styles.times()}>{repeatString}</div>
        <DialogTrigger>
          <CloseButton
            className={styles.timeButtons()}
            tooltip={getText('delete')}
            tooltipPlacement="top left"
          />
          <ConfirmDeleteModal
            actionText={getText('deleteThisProjectExecution')}
            doDelete={async () => {
              await deleteProjectExecution.mutateAsync([projectExecution.executionId, item.title])
            }}
          />
        </DialogTrigger>
      </div>
      <ButtonGroup className={styles.optionContainer()}>
        <Button
          size="xsmall"
          variant="outline"
          icon={TimeIcon}
          tooltip={getText('maxDurationLabel')}
          tooltipPlacement="left"
          className={styles.maximumDuration()}
        >
          {getText('xMinutes', projectExecution.maxDurationMinutes)}
        </Button>
        <Button
          size="xsmall"
          variant="outline"
          icon={RepeatIcon}
          tooltip={getText('repeatIntervalLabel')}
          tooltipPlacement="left"
          className={styles.repeatInterval()}
        >
          {getText(
            backendModule.PROJECT_EXECUTION_REPEAT_TYPE_TO_TEXT_ID[projectExecution.repeat.type],
          )}
        </Button>
      </ButtonGroup>
    </div>
  )
}
