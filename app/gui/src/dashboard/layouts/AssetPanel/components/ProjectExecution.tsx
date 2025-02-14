/** @file Displays information describing a specific version of an asset. */
import { useMutation } from '@tanstack/react-query'

import {
  DAY_3_LETTER_TEXT_IDS,
  DAY_TEXT_IDS,
  DAYS_PER_WEEK,
  MONTH_3_LETTER_TEXT_IDS,
} from 'enso-common/src/utilities/data/dateTime'

import RepeatIcon from '#/assets/repeat.svg'
import TimeIcon from '#/assets/time.svg'
import { DialogTrigger } from '#/components/aria'
import { Button, ButtonGroup, CloseButton, WithVisualTooltip } from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useGetOrdinal } from '#/hooks/ordinalHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { getLocalTimeZone, parseAbsolute, type ZonedDateTime } from '@internationalized/date'

const MONTHS_IN_YEAR = 12

const PROJECT_EXECUTION_STYLES = tv({
  base: 'group flex flex-row gap-1 w-full rounded-default items-center odd:bg-primary/5 p-2',
  variants: {
    isEnabled: { false: { time: 'opacity-50', optionContainer: 'opacity-50' } },
    compact: { true: { base: 'px-2' } },
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
  readonly compact?: boolean
  readonly backend: Backend
  readonly item: backendModule.ProjectAsset
  readonly projectExecution: backendModule.ProjectExecution
  /** Defaults to the first date of `projectExecution` if not given. */
  readonly date?: ZonedDateTime
}

/** Displays information describing a specific version of an asset. */
export function ProjectExecution(props: ProjectExecutionProps) {
  const { compact = false, backend, item, projectExecution, date } = props
  const { getText } = useText()
  const getOrdinal = useGetOrdinal()
  const [timeZone = getLocalTimeZone()] = useLocalStorageState('preferredTimeZone')
  const { repeat } = projectExecution

  const repeatString = (() => {
    if (date) {
      const minuteString = String(date.minute).padStart(2, '0')
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      return getText(date.hour > 11 ? 'xPm' : 'xAm', `${date.hour % 12 || 12}:${minuteString}`)
    } else {
      const zonedStartDate = parseAbsolute(projectExecution.startDate, timeZone)
      const minuteString = String(zonedStartDate.minute).padStart(2, '0')
      const startDateDailyRepeat = getText(
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        zonedStartDate.hour > 11 ? 'xPm' : 'xAm',
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        `${zonedStartDate.hour % 12 || 12}:${minuteString}`,
      )
      switch (repeat.type) {
        case 'none': {
          return getText('doesNotRepeat')
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
    compact,
    isEnabled: projectExecution.enabled,
  })

  const deleteProjectExecution = useMutation(
    backendMutationOptions(backend, 'deleteProjectExecution'),
  )

  const timeEl = (
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
  )

  const maxDurationLabel = getText('maxDurationLabel')
  const maxDurationDescription = getText('xMinutes', projectExecution.maxDurationMinutes)
  const repeatIntervalLabel = getText('repeatIntervalLabel')
  const repeatIntervalDescription = getText(
    backendModule.PROJECT_EXECUTION_REPEAT_TYPE_TO_TEXT_ID[projectExecution.repeat.type],
  )

  return (
    <div className={styles.base()}>
      {compact && (
        <WithVisualTooltip
          tooltip={`${maxDurationLabel}: ${maxDurationDescription}\n${repeatIntervalLabel}: ${repeatIntervalDescription}`}
          tooltipPlacement="left"
        >
          {timeEl}
        </WithVisualTooltip>
      )}
      {!compact && timeEl}
      {!compact && (
        <ButtonGroup className={styles.optionContainer()}>
          <Button
            size="xsmall"
            variant="outline"
            icon={TimeIcon}
            tooltip={maxDurationLabel}
            tooltipPlacement="left"
            className={styles.maximumDuration()}
          >
            {maxDurationDescription}
          </Button>
          <Button
            size="xsmall"
            variant="outline"
            icon={RepeatIcon}
            tooltip={repeatIntervalLabel}
            tooltipPlacement="left"
            className={styles.repeatInterval()}
          >
            {repeatIntervalDescription}
          </Button>
        </ButtonGroup>
      )}
    </div>
  )
}
