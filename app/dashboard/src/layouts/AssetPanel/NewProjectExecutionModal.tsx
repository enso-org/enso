/** @file Modal for confirming delete of any type of asset. */
import * as z from 'zod'

import type Backend from '#/services/Backend'
import {
  PROJECT_PARALLEL_MODES,
  PROJECT_REPEAT_INTERVALS,
  type ProjectAsset,
  type ProjectRepeatInterval,
} from 'enso-common/src/services/Backend'
import type { TextId } from 'enso-common/src/text'

import {
  Button,
  ButtonGroup,
  Dialog,
  Form,
  Input,
  MultiSelector,
  Selector,
} from '#/components/AriaComponents'
import { useText } from '#/providers/TextProvider'

const REPEAT_INTERVAL_TO_TEXT = {
  hourly: 'hourlyRepeatInterval',
  daily: 'dailyRepeatInterval',
  weekly: 'weeklyRepeatInterval',
  monthly: 'monthlyRepeatInterval',
} satisfies {
  [K in ProjectRepeatInterval]: TextId & `${K}RepeatInterval`
}

const DAY_TEXT_ID = [
  'monday3',
  'tuesday3',
  'wednesday3',
  'thursday3',
  'friday3',
  'saturday3',
  'sunday3',
] satisfies TextId[]

/* eslint-disable @typescript-eslint/no-magic-numbers */
const DATES = [...Array(31).keys()]
const DAYS = [...Array(7).keys()]
const HOURS = [...Array(24).keys()]

/** Create the form schema for this page. */
function createUpsertExecutionSchema() {
  return z.object({
    repeatInterval: z.enum(PROJECT_REPEAT_INTERVALS),
    dates: z.number().int().min(0).max(30).array().readonly(),
    days: z.number().int().min(0).max(6).array().readonly(),
    hours: z.number().int().min(0).max(23).array().readonly(),
    minute: z.number().int().min(0).max(59),
    parallelMode: z.enum(PROJECT_PARALLEL_MODES),
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
}

/** A modal for confirming the deletion of an asset. */
export default function NewProjectExecutionModal(props: NewProjectExecutionModalProps) {
  const { backend, item } = props
  const { getText } = useText()

  const form = Form.useForm({ schema: createUpsertExecutionSchema() })

  const repeatInterval = form.watch('repeatInterval')

  return (
    <Dialog title={getText('newProjectExecution')}>
      {({ close }) => (
        <Form
          form={form}
          className="w-full"
          onSubmit={async ({
            repeatInterval: newRepeatInterval,
            dates,
            days,
            hours,
            minute,
            parallelMode,
          }) => {
            await backend.createProjectExecution(
              {
                projectId: item.id,
                repeatInterval: newRepeatInterval,
                times: { dates, days, hours, minute },
                parallelMode,
              },
              item.title,
            )
          }}
        >
          <Selector
            form={form}
            name="repeatInterval"
            label={getText('repeatIntervalLabel')}
            defaultValue={'weekly'}
            items={PROJECT_REPEAT_INTERVALS}
            itemToString={(interval) => getText(REPEAT_INTERVAL_TO_TEXT[interval])}
          />
          {repeatInterval === 'monthly' && (
            <MultiSelector
              form={form}
              name="dates"
              label={getText('datesLabel')}
              items={DATES}
              itemToString={(n) => String(n + 1)}
              columns={10}
            />
          )}
          {repeatInterval === 'weekly' && (
            <MultiSelector
              form={form}
              name="days"
              label={getText('daysLabel')}
              items={DAYS}
              itemToString={(n) => getText(DAY_TEXT_ID[n] ?? 'monday3')}
            />
          )}
          {repeatInterval !== 'hourly' && (
            <MultiSelector
              form={form}
              name="hours"
              label={getText('hoursLabel')}
              items={HOURS}
              columns={12}
            />
          )}
          <Input
            readOnly
            name="minute"
            label={getText('minuteLabel')}
            type="number"
            min={0}
            max={59}
          />
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
