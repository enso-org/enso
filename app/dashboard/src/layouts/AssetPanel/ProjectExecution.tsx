/** @file Displays information describing a specific version of an asset. */
import { useMutation } from '@tanstack/react-query'

import { DAY_TEXT_IDS } from 'enso-common/src/utilities/data/dateTime'

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

const PROJECT_EXECUTION_STYLES = tv({
  base: 'flex flex-row w-full items-center',
  slots: {
    timeContainer: 'group flex flex-row items-center gap-2 grow px-2 py-0.5',
    time: '',
    timeButtons: 'opacity-0 group-hover:opacity-100 transition-[opacity]',
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
  const time = projectExecution.time
  const minuteString = time.minute === 0 ? '' : `:${String(time.minute).padStart(2, '0')}`
  const timeString =
    time.hour != null ?
      // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      time.hour > 11 ?
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
        `${time.hour % 12 || 12}${minuteString} pm`
        // eslint-disable-next-line @typescript-eslint/no-magic-numbers
      : `${time.hour || 12}${minuteString} am`
    : `xx${minuteString || ':00'}`

  const styles = PROJECT_EXECUTION_STYLES({})

  const deleteProjectExecution = useMutation(
    backendMutationOptions(backend, 'deleteProjectExecution'),
  ).mutateAsync

  return (
    <div className={styles.base()}>
      <div className={styles.timeContainer()}>
        <Text elementType="time" className={styles.time()}>
          {time.date != null && `${time.date + 1} `}
          {time.day != null && `${getText(DAY_TEXT_IDS[time.day] ?? 'monday')} `}
          {timeString}
        </Text>
        <DialogTrigger>
          <CloseButton
            className={styles.timeButtons()}
            tooltip={getText('delete')}
            tooltipPlacement="top left"
          />
          <ConfirmDeleteModalNew
            actionText={getText('deleteThisProjectExecution')}
            doDelete={async () => {
              await deleteProjectExecution([projectExecution.projectExecutionId, item.title])
            }}
          />
        </DialogTrigger>
      </div>
      <ButtonGroup className={styles.optionContainer()}>
        <Button
          size="xsmall"
          variant="outline"
          icon={RepeatIcon}
          tooltip={getText('repeatIntervalLabel')}
          className={styles.optionDisplay()}
        >
          {getText(backendModule.REPEAT_INTERVAL_TO_TEXT_ID[projectExecution.repeatInterval])}
        </Button>
        <Button
          size="xsmall"
          variant="outline"
          tooltip={getText('parallelModeLabel')}
          icon={ParallelIcon}
          className={styles.optionDisplay()}
        >
          {getText(backendModule.PARALLEL_MODE_TO_TEXT_ID[projectExecution.parallelMode])}
        </Button>
      </ButtonGroup>
    </div>
  )
}
