/** @file Displays information describing a specific version of an asset. */
import { useMutation, useQuery } from '@tanstack/react-query'

import {
  DAY_3_LETTER_TEXT_IDS,
  DAY_TEXT_IDS,
  getDescriptionForTimeZone,
  getTimeZoneOffsetStringWithGMT,
  MONTH_3_LETTER_TEXT_IDS,
} from 'enso-common/src/utilities/data/dateTime'

import LogsIcon from '#/assets/logs.svg'
import RepeatIcon from '#/assets/repeat.svg'
import TimeIcon from '#/assets/time.svg'
import { DialogTrigger } from '#/components/aria'
import { Button, ButtonGroup, CloseButton, Text, VisualTooltip } from '#/components/AriaComponents'
import {
  backendMutationOptions,
  getProjectExecutionDetailsQueryOptions,
} from '#/hooks/backendHooks'
import { useGetOrdinal } from '#/hooks/ordinalHooks'
import ConfirmDeleteModal from '#/modals/ConfirmDeleteModal'
import ProjectLogsModal from '#/modals/ProjectLogsModal'
import { useFeatureFlag } from '#/providers/FeatureFlagsProvider'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { getLocalTimeZone, now, parseAbsolute, type ZonedDateTime } from '@internationalized/date'

/** The maximum duration, in milliseconds, between two dates to be considered the same project execution. */
const EXECUTION_TIME_DIFFERENCE_THRESHOLD_MS = 60_000
const MONTHS_IN_YEAR = 12

const PROJECT_EXECUTION_STYLES = tv({
  base: 'group flex flex-row gap-1 w-full rounded-default items-center odd:bg-primary/5 p-2',
  variants: {
    isEnabled: { false: { time: 'opacity-50', infoContainer: 'opacity-50' } },
    compact: { true: { base: 'px-2' } },
  },
  slots: {
    timeContainer: 'flex flex-row items-center gap-2 grow px-2 py-0.5',
    times: 'flex flex-col max-h-[10lh] overflow-auto grow',
    time: '',
    timeButtons: 'opacity-0 group-hover:opacity-100 transition-[opacity]',
    infoContainer: 'flex flex-col grow-0 gap-1',
    info: 'cursor-default max-w-48 hover:bg-transparent',
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
  const enableAdvancedProjectExecutionOptions = useFeatureFlag(
    'enableAdvancedProjectExecutionOptions',
  )
  const { repeat } = projectExecution

  const { data: details } = useQuery(
    getProjectExecutionDetailsQueryOptions(backend, projectExecution.executionId, item.title),
  )

  const sessions = details?.projectSessions
  const session =
    date == null ? null : (
      sessions?.find(
        (otherSession) =>
          Number(new Date(otherSession.createdAt)) - Number(date.toDate()) <
          EXECUTION_TIME_DIFFERENCE_THRESHOLD_MS,
      )
    )

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
          return `${startDateDailyRepeat} ${getText('everyDaySuffix')}`
        }
        case 'weekly': {
          const dayNames = repeat.daysOfWeek
            .map((day) => getText(DAY_3_LETTER_TEXT_IDS[day] ?? 'monday3'))
            .join(', ')
          return `${startDateDailyRepeat} ${dayNames}`
        }
        case 'monthlyDate':
        case 'monthlyWeekday':
        case 'monthlyLastWeekday': {
          const monthNames =
            repeat.months.length === MONTHS_IN_YEAR ?
              getText('everyMonth')
            : repeat.months
                .map((month) => getText(MONTH_3_LETTER_TEXT_IDS[month] ?? 'january3'))
                .join(', ')
          switch (repeat.type) {
            case 'monthlyDate': {
              return getText(
                'repeatsTimeXMonthsXDateX',
                startDateDailyRepeat,
                monthNames,
                getOrdinal(repeat.date),
              )
            }
            case 'monthlyWeekday': {
              return getText(
                'repeatsTimeXMonthsXDayXWeekX',
                startDateDailyRepeat,
                monthNames,
                getText(DAY_TEXT_IDS[repeat.dayOfWeek] ?? 'monday'),
                getText('xthWeek', getOrdinal(repeat.weekNumber)),
              )
            }
            case 'monthlyLastWeekday': {
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
    isEnabled: true,
  })

  const deleteProjectExecution = useMutation(
    backendMutationOptions(backend, 'deleteProjectExecution'),
  )

  const repeatEl = <div>{repeatString}</div>

  const maxDurationLabel = getText('maxDurationLabel')
  const maxDurationDescription = getText('xMinutes', projectExecution.maxDurationMinutes)
  const repeatIntervalLabel = getText('repeatIntervalLabel')
  const repeatIntervalDescription = getText(
    backendModule.PROJECT_EXECUTION_REPEAT_TYPE_TO_TEXT_ID[projectExecution.repeat.type],
  )
  const timeZoneLabel = getText('timeZoneLabel')
  const timeZoneDescription = `${getTimeZoneOffsetStringWithGMT(now(projectExecution.timeZone))} ${getDescriptionForTimeZone(projectExecution.timeZone)}`

  return (
    <div className={styles.base()}>
      <div className={styles.timeContainer()}>
        {!compact ?
          repeatEl
        : <VisualTooltip
            tooltip={
              <div>
                {enableAdvancedProjectExecutionOptions && (
                  <Text color="inherit">{`${maxDurationLabel}: ${maxDurationDescription}`}</Text>
                )}
                <Text color="inherit">{`${repeatIntervalLabel}: ${repeatIntervalDescription}`}</Text>
                <Text color="inherit">{`${timeZoneLabel}: ${timeZoneDescription}`}</Text>
              </div>
            }
            tooltipPlacement="left"
            className={styles.times()}
          >
            {repeatEl}
          </VisualTooltip>
        }
        {session && (
          <DialogTrigger>
            <Button variant="icon" isActive icon={LogsIcon} aria-label={getText('showLogs')} />

            <ProjectLogsModal
              backend={backend}
              projectSessionId={session.projectSessionId}
              projectTitle={item.title}
            />
          </DialogTrigger>
        )}
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
      {!compact && (
        <ButtonGroup className={styles.infoContainer()}>
          {enableAdvancedProjectExecutionOptions && (
            <Button
              size="xsmall"
              variant="outline"
              icon={TimeIcon}
              tooltip={maxDurationLabel}
              tooltipPlacement="left"
              className={styles.info()}
            >
              {maxDurationDescription}
            </Button>
          )}
          <Button
            size="xsmall"
            variant="outline"
            icon={RepeatIcon}
            tooltip={repeatIntervalLabel}
            tooltipPlacement="left"
            className={styles.info()}
          >
            {repeatIntervalDescription}
          </Button>
          <Button
            size="xsmall"
            variant="outline"
            icon="time"
            tooltip={timeZoneLabel}
            tooltipPlacement="left"
            className={styles.info()}
          >
            {timeZoneDescription}
          </Button>
        </ButtonGroup>
      )}
    </div>
  )
}
