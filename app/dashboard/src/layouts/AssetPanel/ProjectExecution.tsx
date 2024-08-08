/** @file Displays information describing a specific version of an asset. */
import type { TextId } from 'enso-common/src/text'

import { Input, Text } from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'
import type * as backendModule from '#/services/Backend'

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

  return (
    <div className="flex w-full flex-1 shrink-0 select-none flex-row gap-4 rounded-2xl p-2">
      <div className="flex flex-1 flex-col">
        <Text>{getText(REPEAT_INTERVAL_TO_TEXT[projectExecution.repeatInterval])}</Text>
        {projectExecution.times.dates &&
          projectExecution.times.dates.map((time) => <time className="text-xs">{time}</time>)}
        {projectExecution.times.days &&
          projectExecution.times.days.map((time) => <time className="text-xs">{time}</time>)}
        {projectExecution.times.hours &&
          projectExecution.times.hours.map((time) => <time className="text-xs">{time}</time>)}
        {projectExecution.times.minute && (
          <Input
            readOnly
            label={getText('minuteLabel')}
            name="minute"
            value={projectExecution.times.minute}
          />
        )}
      </div>
    </div>
  )
}
