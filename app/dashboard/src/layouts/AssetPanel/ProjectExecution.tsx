/** @file Displays information describing a specific version of an asset. */
import { useMemo } from 'react'

import type { TextId } from 'enso-common/src/text'

import { Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type * as backendModule from '#/services/Backend'
import { DAY_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'

const REPEAT_INTERVAL_TO_TEXT = {
  hourly: 'hourlyRepeatInterval',
  daily: 'dailyRepeatInterval',
  weekly: 'weeklyRepeatInterval',
  monthly: 'monthlyRepeatInterval',
} satisfies {
  [K in backendModule.ProjectRepeatInterval]: TextId & `${K}RepeatInterval`
}

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
    const { dates, days, hours = dates || days ? [0] : undefined, minute } = projectExecution.times
    let result: { date?: number; day?: number; hour?: number; minute: number }[] = [{ minute }]
    if (hours) {
      result = hours.flatMap((hour) => result.map((time) => ({ ...time, hour })))
    }
    if (days) {
      result = days.flatMap((day) => result.map((time) => ({ ...time, day })))
    } else if (dates) {
      result = dates.flatMap((date) => result.map((time) => ({ ...time, date })))
    }
    return result
  }, [projectExecution.times])

  return (
    <div className="w-full rounded-2xl border-0.5 border-primary/20 p-2">
      <Text>{getText(REPEAT_INTERVAL_TO_TEXT[projectExecution.repeatInterval])}</Text>
      {times.map(({ date, day, hour, minute }) => {
        const minuteString = minute === 0 ? '' : `:${String(minute).padStart(2, '0')}`
        const timeString =
          (date != null ? date + ' ' : '') +
          (day != null ? getText(DAY_TEXT_IDS[day] ?? 'monday3') + ' ' : '') +
          (hour != null ?
            // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            hour > 11 ?
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
              `${hour % 12 || 12}${minuteString} pm`
              // eslint-disable-next-line @typescript-eslint/no-magic-numbers
            : `${hour || 12}${minuteString} am`
          : `xx${minuteString || ':00'}`)
        return (
          <Text>
            <time className="text-xs">{timeString}</time>
          </Text>
        )
      })}
    </div>
  )
}
