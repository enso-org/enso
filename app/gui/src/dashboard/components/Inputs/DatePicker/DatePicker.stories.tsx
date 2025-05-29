import { roundedVariants } from '#/components/utilities'
import { StoryVariants } from '#/utilities/StoryVariants'
import { CalendarDate, ZonedDateTime, now, type DateValue } from '@internationalized/date'
import type { Meta, StoryObj } from '@storybook/react'
import { z } from 'zod'
import { Form, type FieldPath, type TSchema } from '../../Form'
import type { DatePickerProps } from './DatePicker'
import { DatePicker } from './DatePicker'

type Props = DatePickerProps<typeof schema, 'value'>
type Story = StoryObj<Props>

const schema = z.object({ value: z.instanceof(ZonedDateTime).or(z.instanceof(CalendarDate)) })

const sizes = ['medium', 'small'] as const
const roundeds = roundedVariants()

const args = { name: 'value' }

function DatePickerWrapper<Schema extends TSchema, FieldName extends FieldPath<Schema, DateValue>>(
  props: DatePickerProps<Schema, FieldName>,
) {
  return <DatePicker {...props} />
}

export default {
  title: 'Components/Inputs/DatePicker',
  component: DatePicker,
  tags: ['autodocs'],
  decorators: [
    (Story, context) => (
      <Form schema={schema} className="text-primary">
        {Story(context)}
      </Form>
    ),
  ],
  args,
  parameters: {
    layout: 'centered',
  },
} as Meta<Props>

export const Default: Story = {}

export const WithTime: Story = {
  args: { name: 'value', defaultValue: now('Etc/GMT+0') } satisfies Props,
}

export const WithoutTimeZone: Story = {
  args: { name: 'value', defaultValue: now('Etc/GMT+0'), hideTimeZone: true } satisfies Props,
}

export const Rounded: Story = {
  render: () => (
    <StoryVariants
      columns="3"
      render={DatePickerWrapper}
      toProps={(rounded) => ({ ...args, rounded })}
      variants={roundeds}
    />
  ),
}

export const Size: Story = {
  render: () => (
    <StoryVariants
      render={DatePickerWrapper}
      toProps={(size) => ({ ...args, size })}
      variants={sizes}
    />
  ),
}
