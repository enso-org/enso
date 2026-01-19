/** @file A calendar showing executions of a project. */
import { useState } from 'react'

import CalendarIcon from '#/assets/calendar_repeat_outline.svg'
import ArrowIcon from '#/assets/folder_arrow.svg'
import {
  Calendar,
  CalendarCell,
  CalendarGrid,
  CalendarGridBody,
  CalendarGridHeader,
  CalendarHeaderCell,
  Heading,
} from '#/components/aria'
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { ErrorBoundary } from '#/components/ErrorBoundary'
import { Form } from '#/components/Form'
import { Text } from '#/components/Text'
import { listProjectExecutionsQueryOptions } from '#/hooks/backendHooks'
import { useLocalStorageState } from '#/hooks/localStoreState'
import { AssetPanelPlaceholder } from '#/layouts/AssetPanel/components/AssetPanelPlaceholder'
import { ProjectExecution } from '#/layouts/AssetPanel/components/ProjectExecution'
import { NewProjectExecutionModal } from '#/layouts/NewProjectExecutionModal'
import { tv } from '#/utilities/tailwindVariants'
import { useBackends, useText } from '$/providers/react'
import {
  useRightPanelContextCategory,
  useRightPanelFocusedAsset,
} from '$/providers/react/container'
import {
  CalendarDate,
  getLocalTimeZone,
  now,
  startOfMonth,
  toCalendarDate,
  today,
  toZoned,
  type ZonedDateTime,
} from '@internationalized/date'
import { useSuspenseQuery } from '@tanstack/react-query'
import type { Backend } from 'enso-common/src/services/Backend'
import {
  AssetType,
  BackendType,
  type ProjectExecution as BackendProjectExecution,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'
import { getProjectExecutionRepetitionsForDateRange } from 'enso-common/src/services/Backend/projectExecution'

const PROJECT_EXECUTIONS_CALENDAR_STYLES = tv({
  base: '',
  slots: {
    calendarContainer: 'w-full',
    calendarHeader: 'flex items-center mb-2',
    calendarHeading: 'text-base grow text-center',
    calendarGrid: 'w-full table-fixed',
    calendarGridHeader: 'flex',
    calendarGridHeaderCell: '',
    calendarGridBody: '',
    calendarGridCell:
      'text-center px-1 rounded border border-transparent hover:bg-primary/10 outside-visible-range:text-primary/30 disabled:text-primary/30 selected:border-primary/40 h-16 overflow-clip',
  },
})

/** A calendar showing executions of a project. */
export function ProjectExecutionsCalendar() {
  const { getText } = useText()
  const { remoteBackend } = useBackends()
  const focusedAsset = useRightPanelFocusedAsset()
  const category = useRightPanelContextCategory()

  if (category?.backend !== BackendType.remote) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutionsCalendar.localBackend')} />
  }
  if (focusedAsset == null) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutionsCalendar.notSelected')} />
  }
  if (focusedAsset.type !== AssetType.project) {
    return (
      <AssetPanelPlaceholder title={getText('assetProjectExecutionsCalendar.notProjectAsset')} />
    )
  }
  return (
    <ErrorBoundary>
      <ProjectExecutionsCalendarInternal backend={remoteBackend} item={focusedAsset} />
    </ErrorBoundary>
  )
}

/** Props for a {@link ProjectExecutionsCalendarInternal}. */
interface ProjectExecutionsCalendarInternalProps {
  readonly backend: Backend
  readonly item: ProjectAsset
}

/** A calendar showing executions of a project. */
function ProjectExecutionsCalendarInternal(props: ProjectExecutionsCalendarInternalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const [preferredTimeZone] = useLocalStorageState('preferredTimeZone')

  const form = Form.useForm({
    schema: (z) => z.object({ date: z.instanceof(CalendarDate) }),
    onSubmit: () => {},
  })
  const timeZone = preferredTimeZone ?? getLocalTimeZone()
  const [focusedMonth, setFocusedMonth] = useState(() => startOfMonth(today(timeZone)))
  const todayDate = today(timeZone)
  const selectedDate = Form.useWatch({
    control: form.control,
    name: 'date',
    defaultValue: todayDate,
  })

  const projectExecutionsQuery = useSuspenseQuery(
    listProjectExecutionsQueryOptions(backend, item.id, item.title),
  )
  const projectExecutions = projectExecutionsQuery.data

  const start = startOfMonth(focusedMonth)
  const startDate = toZoned(start, timeZone)
  const end = startOfMonth(focusedMonth.add({ months: 1 }))
  const endDate = toZoned(end, timeZone)
  const projectExecutionsByDate: Record<
    string,
    { readonly date: ZonedDateTime; readonly projectExecution: BackendProjectExecution }[]
  > = {}

  for (const projectExecution of projectExecutions) {
    for (const date of getProjectExecutionRepetitionsForDateRange(
      projectExecution,
      startDate,
      endDate,
    )) {
      const dateString = toCalendarDate(date).toString()
      ;(projectExecutionsByDate[dateString] ??= []).push({ date, projectExecution })
    }
  }
  for (const key in projectExecutionsByDate) {
    projectExecutionsByDate[key]?.sort((a, b) => Number(a.date) - Number(b.date))
  }

  const projectExecutionsForToday = projectExecutions
    .flatMap((projectExecution) =>
      getProjectExecutionRepetitionsForDateRange(
        projectExecution,
        toZoned(selectedDate, projectExecution.timeZone),
        toZoned(selectedDate.add({ days: 1 }), projectExecution.timeZone),
      ).flatMap((date) => ({ date, projectExecution })),
    )
    .sort((a, b) => Number(a.date) - Number(b.date))

  const styles = PROJECT_EXECUTIONS_CALENDAR_STYLES({})

  return (
    <Form
      form={form}
      className="pointer-events-auto flex w-full flex-col items-center gap-2 self-start overflow-y-auto overflow-x-hidden"
    >
      <Form.Controller
        control={form.control}
        name="date"
        render={(renderProps) => (
          <Calendar
            focusedValue={focusedMonth}
            onFocusChange={setFocusedMonth}
            className={styles.calendarContainer()}
            {...renderProps.field}
          >
            <header className={styles.calendarHeader()}>
              <Button variant="icon" slot="previous" icon={ArrowIcon} className="rotate-180" />
              <Heading className={styles.calendarHeading()} />
              <Button variant="icon" slot="next" icon={ArrowIcon} />
            </header>
            <CalendarGrid className={styles.calendarGrid()}>
              <CalendarGridHeader className={styles.calendarGridHeader()}>
                {() => <CalendarHeaderCell className={styles.calendarGridHeaderCell()} />}
              </CalendarGridHeader>
              <CalendarGridBody className={styles.calendarGridBody()}>
                {(date) => {
                  const isToday = date.compare(todayDate) === 0
                  const todaysExecutions = projectExecutionsByDate[date.toString()]
                  return (
                    <CalendarCell
                      key={date.toString()}
                      date={date}
                      className={styles.calendarGridCell()}
                    >
                      <div className="flex flex-col items-center">
                        <Text
                          weight={isToday ? 'bold' : 'medium'}
                          color={isToday ? 'success' : 'inherit'}
                        >
                          {date.day}
                        </Text>
                        {todaysExecutions && (
                          <Button
                            slot={null}
                            isDisabled
                            tooltip={getText(
                              'xExecutionsScheduledOnX',
                              todaysExecutions.length,
                              date.toString(),
                            )}
                            size="xxsmall"
                            variant="custom"
                            className="disabled:cursor-unset disabled:opacity-100"
                            icon={CalendarIcon}
                          >
                            {todaysExecutions.length}
                          </Button>
                        )}
                      </div>
                    </CalendarCell>
                  )
                }}
              </CalendarGridBody>
            </CalendarGrid>
          </Calendar>
        )}
      />
      <Dialog.Trigger>
        <Button variant="outline">{getText('newProjectExecution')}</Button>
        <NewProjectExecutionModal
          backend={backend}
          item={item}
          defaultDate={toZoned(selectedDate, timeZone).set({ hour: now(timeZone).hour })}
        />
      </Dialog.Trigger>
      <Text>{getText('projectSessionsOnX', selectedDate.toString())}</Text>
      {projectExecutionsForToday.length === 0 ?
        <Text color="disabled">{getText('noProjectExecutions')}</Text>
      : projectExecutionsForToday.map(({ projectExecution, date }) => (
          <ProjectExecution
            key={projectExecution.executionId}
            compact
            backend={backend}
            item={item}
            projectExecution={projectExecution}
            date={date}
          />
        ))
      }
    </Form>
  )
}
