/** @file Displays information describing a specific version of an asset. */
import { Fragment, useMemo } from 'react'

import ParallelIcon from '#/assets/parallel.svg'
import RepeatIcon from '#/assets/repeat.svg'
import { DialogTrigger } from '#/components/aria'
import { Button, ButtonGroup, CloseButton, Text } from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import ConfirmDeleteModalNew from '#/modals/ConfirmDeleteModalNew'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import * as backendModule from '#/services/Backend'
import { tv } from '#/utilities/tailwindVariants'
import { useMutation } from '@tanstack/react-query'
import { DAY_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'

const PROJECT_EXECUTION_STYLES = tv({
  base: 'relative flex gap-2 w-full items-center rounded-2xl border-0.5 border-primary/20 p-2 pt-6',
  slots: {
    timeTable: 'grid grow items-center justify-start grid-cols-[auto_1fr] gap-x-2 gap-y-1',
    dayOrDate: 'col-start-1',
    timeGrid:
      'col-start-2 mr-auto grid w-full grid-cols-3 rounded-2xl border-0.5 border-primary/20 border-collapse',
    time: 'px-2 py-0.5 text-center',
    optionContainer: 'grow-0',
    optionDisplay: 'cursor-default hover:border-primary/40 hover:bg-transparent',
  },
})

// ========================
// === ProjectExecution ===
// ========================

/** Props for a {@link ProjectExecution}. */
export interface ProjectExecutionProps {
  readonly backend: Backend
  readonly item: backendModule.ProjectAsset
  readonly projectExecution: backendModule.ProjectExecution
}

/** Displays information describing a specific version of an asset. */
export default function ProjectExecution(props: ProjectExecutionProps) {
  const { backend, item, projectExecution } = props
  const { getText } = useText()
  const times = useMemo(() => {
    const hours =
      projectExecution.times.hours ??
      (projectExecution.times.dates || projectExecution.times.days ? [0] : undefined)
    let result: { hour?: number; minute: number }[] = [{ minute: projectExecution.times.minute }]
    if (hours) {
      result = hours.flatMap((hour) => result.map((time) => ({ ...time, hour })))
    }
    return result.map(({ hour, minute }) => {
      const minuteString = minute === 0 ? '' : `:${String(minute).padStart(2, '0')}`
      return (
        hour != null ?
          // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          hour > 11 ?
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            `${hour % 12 || 12}${minuteString} pm`
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
          : `${hour || 12}${minuteString} am`
        : `xx${minuteString || ':00'}`
      )
    })
  }, [projectExecution.times])

  const styles = PROJECT_EXECUTION_STYLES({})

  const deleteProjectExecution = useMutation(
    backendMutationOptions(backend, 'deleteProjectExecution'),
  ).mutateAsync

  return (
    <div className={styles.base()}>
      <DialogTrigger>
        <CloseButton
          className="absolute left-2 top-2"
          tooltip={getText('delete')}
          tooltipPlacement="right"
        />
        <ConfirmDeleteModalNew
          actionText={getText('deleteThisProjectExecution')}
          doDelete={async () => {
            await deleteProjectExecution([projectExecution.projectExecutionId, item.title])
          }}
        />
      </DialogTrigger>
      <div className={styles.timeTable()}>
        {projectExecution.times.dates?.flatMap((date) => (
          <Fragment key={date}>
            <Text elementType="time" className={styles.dayOrDate()}>
              {date + 1}
            </Text>
            <div className={styles.timeGrid()}>
              {times.map((timeString) => (
                <Text elementType="time" className={styles.time()}>
                  {timeString}
                </Text>
              ))}
            </div>
          </Fragment>
        ))}
        {projectExecution.times.days?.flatMap((day) => (
          <Fragment key={day}>
            <Text elementType="time" className={styles.dayOrDate()}>
              {getText(DAY_TEXT_IDS[day] ?? 'monday')}
            </Text>
            <div className={styles.timeGrid()}>
              {times.map((timeString) => (
                <Text elementType="time" className={styles.time()}>
                  {timeString}
                </Text>
              ))}
            </div>
          </Fragment>
        ))}
      </div>
      <ButtonGroup direction="column" className={styles.optionContainer()}>
        <Button
          variant="outline"
          icon={RepeatIcon}
          tooltip={getText('repeatIntervalLabel')}
          tooltipPlacement="left"
          className={styles.optionDisplay()}
        >
          {getText(backendModule.REPEAT_INTERVAL_TO_TEXT_ID[projectExecution.repeatInterval])}
        </Button>
        <Button
          variant="outline"
          tooltip={getText('parallelModeLabel')}
          tooltipPlacement="left"
          icon={ParallelIcon}
          className={styles.optionDisplay()}
        >
          {getText(backendModule.PARALLEL_MODE_TO_TEXT_ID[projectExecution.parallelMode])}
        </Button>
      </ButtonGroup>
    </div>
  )
}
