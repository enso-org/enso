/** @file A calendar showing executions of a project. */
import { useState } from 'react'

import {
  CalendarDate,
  getLocalTimeZone,
  parseAbsolute,
  startOfMonth,
  toCalendarDate,
  today,
  toZoned,
} from '@internationalized/date'
import { useSuspenseQuery } from '@tanstack/react-query'

import { getProjectExecutionRepetitionsForDateRange } from 'enso-common/src/services/Backend/projectExecution'

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
import { Button, ButtonGroup, Form, Text } from '#/components/AriaComponents'
import { useStore } from '#/hooks/storeHooks'
import { assetPanelStore } from '#/layouts/AssetPanel/AssetPanelState'
import { AssetPanelPlaceholder } from '#/layouts/AssetPanel/components/AssetPanelPlaceholder'
import { ProjectExecution } from '#/layouts/AssetPanel/components/ProjectExecution'
import { NewProjectExecutionForm } from '#/layouts/NewProjectExecutionModal'
import { useLocalStorageState } from '#/providers/LocalStorageProvider'
import { useText } from '#/providers/TextProvider'
import type Backend from '#/services/Backend'
import {
  AssetType,
  BackendType,
  type ProjectExecution as BackendProjectExecution,
  type ProjectAsset,
  type ProjectExecutionInfo,
} from '#/services/Backend'
import { EMPTY_SET } from '#/utilities/set'
import { tv } from '#/utilities/tailwindVariants'

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

/** Props for a {@link ProjectExecutionsCalendar}. */
export interface ProjectExecutionsCalendarProps {
  readonly backend: Backend
}

/** A calendar showing executions of a project. */
export function ProjectExecutionsCalendar(props: ProjectExecutionsCalendarProps) {
  const { backend } = props
  const { getText } = useText()
  const { item } = useStore(assetPanelStore, (state) => ({ item: state.assetPanelProps.item }), {
    unsafeEnableTransition: true,
  })

  if (backend.type === BackendType.local) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutionsCalendar.localBackend')} />
  }
  if (item == null) {
    return <AssetPanelPlaceholder title={getText('assetProjectExecutionsCalendar.notSelected')} />
  }
  if (item.type !== AssetType.project) {
    return (
      <AssetPanelPlaceholder title={getText('assetProjectExecutionsCalendar.notProjectAsset')} />
    )
  }
  return <ProjectExecutionsCalendarInternal {...props} item={item} />
}

/** Props for a {@link ProjectExecutionsCalendarInternal}. */
interface ProjectExecutionsCalendarInternalProps extends ProjectExecutionsCalendarProps {
  readonly item: ProjectAsset
}

/** A calendar showing executions of a project. */
function ProjectExecutionsCalendarInternal(props: ProjectExecutionsCalendarInternalProps) {
  const { backend, item } = props
  const { getText } = useText()
  const [preferredTimeZone] = useLocalStorageState('preferredTimeZone')

  const [isCreatingExecution, setIsCreatingExecution] = useState(false)
  const [newProjectExecutionInfo, setNewProjectExecutionInfo] =
    useState<ProjectExecutionInfo | null>(null)
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

  const projectExecutionsQuery = useSuspenseQuery({
    queryKey: [backend.type, 'listProjectExecutions', item.id, item.title],
    queryFn: async () => {
      const executions = await backend.listProjectExecutions(item.id, item.title)
      return [...executions].reverse()
    },
  })
  const projectExecutions = projectExecutionsQuery.data

  const start = startOfMonth(focusedMonth)
  const startDate = start.toDate(timeZone)
  const end = startOfMonth(focusedMonth.add({ months: 1 }))
  const endDate = end.toDate(timeZone)
  const projectExecutionsByDate: Record<
    string,
    { readonly date: Date; readonly projectExecution: BackendProjectExecution }[]
  > = {}
  for (const projectExecution of projectExecutions) {
    for (const date of getProjectExecutionRepetitionsForDateRange(
      projectExecution,
      startDate,
      endDate,
    )) {
      const dateString = toCalendarDate(parseAbsolute(date.toISOString(), timeZone)).toString()
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
        selectedDate.toDate(timeZone),
        selectedDate.add({ days: 1 }).toDate(timeZone),
      ).flatMap((date) => ({ date, projectExecution })),
    )
    .sort((a, b) => Number(a.date) - Number(b.date))

  const repeatDaysForCurrentProjectExecution: ReadonlySet<string> =
    isCreatingExecution && newProjectExecutionInfo ?
      new Set(
        getProjectExecutionRepetitionsForDateRange(
          newProjectExecutionInfo,
          startOfMonth(focusedMonth).toDate(timeZone),
          startOfMonth(focusedMonth.add({ months: 1 })).toDate(timeZone),
        ).map((date) => toCalendarDate(parseAbsolute(date.toISOString(), timeZone)).toString()),
      )
    : EMPTY_SET

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
                  return (
                    <CalendarCell date={date} className={styles.calendarGridCell()}>
                      <div className="flex flex-col items-center">
                        <Text
                          weight={isToday ? 'bold' : 'medium'}
                          color={
                            isToday ? 'success'
                            : repeatDaysForCurrentProjectExecution.has(date.toString()) ?
                              'danger'
                            : 'current'
                          }
                        >
                          {date.day}
                        </Text>
                        {projectExecutionsByDate[date.toString()]?.map((data) => (
                          <Text color="disabled">{`${data.date.getHours().toString().padStart(2, '0')}:${data.date.getMinutes().toString().padStart(2, '0')}`}</Text>
                        ))}
                      </div>
                    </CalendarCell>
                  )
                }}
              </CalendarGridBody>
            </CalendarGrid>
          </Calendar>
        )}
      />
      <ButtonGroup>
        <Button
          variant="outline"
          onPress={() => {
            setIsCreatingExecution(!isCreatingExecution)
          }}
        >
          {getText('newProjectExecution')}
        </Button>
      </ButtonGroup>
      {isCreatingExecution ?
        <NewProjectExecutionForm
          backend={backend}
          item={item}
          defaultDate={toZoned(selectedDate, timeZone)}
          onChange={setNewProjectExecutionInfo}
          onCancel={() => {
            setIsCreatingExecution(false)
          }}
        />
      : <>
          <Text>
            {getText(
              'projectSessionsOnX',
              Intl.DateTimeFormat().format(selectedDate.toDate(timeZone)),
            )}
          </Text>
          {projectExecutionsForToday.length === 0 ?
            <Text color="disabled">{getText('noProjectExecutions')}</Text>
          : projectExecutionsForToday.map(({ projectExecution, date }) => (
              <ProjectExecution
                hideDay
                backend={backend}
                item={item}
                projectExecution={projectExecution}
                date={date}
              />
            ))
          }
        </>
      }
    </Form>
  )
}
