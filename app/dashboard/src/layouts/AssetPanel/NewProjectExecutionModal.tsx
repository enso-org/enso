/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import {
  ZonedDateTime,
  getDayOfWeek,
  getLocalTimeZone,
  now,
  toTimeZone,
} from '@internationalized/date'
import { useMutation } from '@tanstack/react-query'

import type Backend from '#/services/Backend'
import {
  PARALLEL_MODE_TO_TEXT_ID,
  PROJECT_PARALLEL_MODES,
  PROJECT_REPEAT_INTERVALS,
  REPEAT_INTERVAL_TO_TEXT_ID,
  type ProjectAsset,
} from 'enso-common/src/services/Backend'

import {
  Button,
  ButtonGroup,
  DatePicker,
  Dialog,
  Form,
  Selector,
} from '#/components/AriaComponents'
import { backendMutationOptions } from '#/hooks/backendHooks'
import { useText, type GetText } from '#/providers/TextProvider'

/* eslint-disable @typescript-eslint/no-magic-numbers */
/** Create the form schema for this page. */
function createUpsertExecutionSchema(getText: GetText) {
  return z
    .object({
      repeatInterval: z.enum(PROJECT_REPEAT_INTERVALS),
      date: z.instanceof(ZonedDateTime, { message: getText('pleaseSelectATime') }),
      parallelMode: z.enum(PROJECT_PARALLEL_MODES),
    })
    .transform(({ repeatInterval, date, parallelMode }) => {
      const utcDate = toTimeZone(date, 'UTC')
      return {
        repeatInterval,
        ...(repeatInterval === 'monthly' && { date: utcDate.day }),
        ...(repeatInterval === 'weekly' && { day: getDayOfWeek(utcDate, 'en-US') }),
        ...(repeatInterval !== 'hourly' && { hour: utcDate.hour }),
        minute: utcDate.minute,
        parallelMode,
      }
    })
}
/* eslint-enable @typescript-eslint/no-magic-numbers */

// ================================
// === NewProjectExecutionModal ===
// ================================

/** Props for a {@link NewProjectExecutionModal}. */
export interface NewProjectExecutionModalProps {
  readonly backend: Backend
  readonly item: ProjectAsset
  readonly defaultOpen?: boolean
}

/** A modal for confirming the deletion of an asset. */
export default function NewProjectExecutionModal(props: NewProjectExecutionModalProps) {
  const { backend, item, defaultOpen } = props
  const { getText } = useText()

  const form = Form.useForm({ schema: createUpsertExecutionSchema(getText) })

  const createProjectExecution = useMutation(
    backendMutationOptions(backend, 'createProjectExecution'),
  ).mutateAsync

  const repeatInterval = form.watch('repeatInterval')
  const minFirstOccurrence = now(getLocalTimeZone())
  const maxFirstOccurrence = (() => {
    switch (repeatInterval) {
      case 'hourly': {
        return minFirstOccurrence.add({ hours: 1 })
      }
      case 'daily': {
        return minFirstOccurrence.add({ days: 1 })
      }
      case 'weekly': {
        return minFirstOccurrence.add({ weeks: 1 })
      }
      case 'monthly': {
        return minFirstOccurrence.add({ months: 1 })
      }
    }
  })()

  return (
    <Dialog title={getText('newProjectExecution')} {...(defaultOpen != null && { defaultOpen })}>
      {({ close }) => (
        <Form
          form={form}
          method="dialog"
          defaultValues={{ repeatInterval: 'weekly', parallelMode: 'restart', minute: 0 }}
          className="w-full"
          onSubmit={async (values) => {
            const { repeatInterval: newRepeatInterval, parallelMode, ...time } = values
            await createProjectExecution([
              { projectId: item.id, repeatInterval: newRepeatInterval, time, parallelMode },
              item.title,
            ])
          }}
        >
          <Selector
            form={form}
            isRequired
            name="repeatInterval"
            label={getText('repeatIntervalLabel')}
            items={PROJECT_REPEAT_INTERVALS}
            itemToString={(interval) => getText(REPEAT_INTERVAL_TO_TEXT_ID[interval])}
          />
          <Selector
            form={form}
            isRequired
            name="parallelMode"
            label={getText('parallelModeLabel')}
            items={PROJECT_PARALLEL_MODES}
            itemToString={(interval) => getText(PARALLEL_MODE_TO_TEXT_ID[interval])}
          />
          <DatePicker
            form={form}
            isRequired
            name="date"
            label={getText('firstOccurrenceLabel')}
            noCalendarHeader
            minValue={minFirstOccurrence}
            maxValue={maxFirstOccurrence}
          />
          {/* FIXME: Show next 5 repeats vertically with opacity-50, and show tooltips on "parallel mode" options */}

          <Form.FormError />
          <ButtonGroup>
            <Form.Submit />
            <Button variant="cancel" onPress={close}>
              {getText('cancel')}
            </Button>
          </ButtonGroup>
        </Form>
      )}
    </Dialog>
  )
}
