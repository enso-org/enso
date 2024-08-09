/** @file Displays information describing a specific version of an asset. */
import { Fragment, useMemo } from 'react'

import ParallelIcon from '#/assets/parallel.svg'
import RepeatIcon from '#/assets/repeat.svg'
import { Button, ButtonGroup, Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import * as backendModule from '#/services/Backend'
import { DAY_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'

// ========================
// === ProjectExecution ===
// ========================

/** Props for a {@link ProjectExecution}. */
export interface ProjectExecutionProps {
  readonly projectExecution: backendModule.ProjectExecution
}

/** Displays information describing a specific version of an asset. */
export default function ProjectExecution(props: ProjectExecutionProps) {
  const { projectExecution } = props
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

  return (
    <div className="flex w-full items-center rounded-2xl border-0.5 border-primary/20 p-2">
      <div className="grid grow items-center justify-start gap-x-2 gap-y-1">
        {projectExecution.times.dates?.flatMap((date) => (
          <Fragment key={date}>
            <Text className="col-start-1">
              <time>{date}</time>
            </Text>
            <div className="col-start-2 mr-auto flex rounded-2xl border-0.5 border-primary/20">
              {times.map((timeString) => (
                <Text>
                  <time>{timeString}</time>
                </Text>
              ))}
            </div>
          </Fragment>
        ))}
        {projectExecution.times.days?.flatMap((day) => (
          <Fragment key={day}>
            <Text>
              <time>{getText(DAY_TEXT_IDS[day] ?? 'monday')}</time>
            </Text>
            <div className="col-start-2 mr-auto flex rounded-2xl border-0.5 border-primary/20">
              {times.map((timeString) => (
                <Text className="border-r-0.5 border-primary/20 px-2 py-0.5 last:border-r-0">
                  <time>{timeString}</time>
                </Text>
              ))}
            </div>
          </Fragment>
        ))}
      </div>
      <ButtonGroup direction="column" className="grow-0">
        <Button
          variant="outline"
          icon={RepeatIcon}
          tooltip={getText('repeatIntervalLabel')}
          tooltipPlacement="left"
          className="cursor-default hover:border-primary/40 hover:bg-transparent"
        >
          {getText(backendModule.REPEAT_INTERVAL_TO_TEXT_ID[projectExecution.repeatInterval])}
        </Button>
        <Button
          variant="outline"
          tooltip={getText('parallelModeLabel')}
          tooltipPlacement="left"
          icon={ParallelIcon}
          className="cursor-default hover:border-primary/40 hover:bg-transparent"
        >
          {getText(backendModule.PARALLEL_MODE_TO_TEXT_ID[projectExecution.parallelMode])}
        </Button>
      </ButtonGroup>
    </div>
  )
}
