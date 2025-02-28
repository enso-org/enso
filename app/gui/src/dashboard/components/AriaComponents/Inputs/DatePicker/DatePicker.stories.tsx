import { Text } from '#/components/AriaComponents/Text'
import { roundedVariants } from '#/components/AriaComponents/utilities'
import { CalendarDate, ZonedDateTime, now } from '@internationalized/date'
import type { Meta, StoryObj } from '@storybook/react'
import { z } from 'zod'
import { Form } from '../../Form/index'
import type { DatePickerProps } from './DatePicker'
import { DatePicker } from './DatePicker'

type Props = DatePickerProps<typeof schema, 'value'>
type Story = StoryObj<Props>

const schema = z.object({ value: z.instanceof(ZonedDateTime).or(z.instanceof(CalendarDate)) })

const sizes = ['medium', 'small'] as const
const roundeds = roundedVariants()

export default {
  title: 'Components/Inputs/DatePicker',
  component: DatePicker,
  render: (args) => <DatePicker {...args} />,
  tags: ['autodocs'],
  decorators: [
    (Story, context) => (
      <Form schema={schema} className="text-primary">
        {Story(context)}
      </Form>
    ),
  ],
  args: { name: 'value' },
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
  render: (_Story, context) => (
    <div className="grid grid-cols-3 gap-4">
      {roundeds.map((rounded) => (
        <div key={rounded} className="flex flex-col items-center gap-1">
          <DatePicker {...context.args} rounded={rounded} />
          <Text variant="caption">{rounded}</Text>
        </div>
      ))}
    </div>
  ),
}

export const Size: Story = {
  render: (_Story, context) => (
    <div className="around grid grid-cols-1 gap-4">
      {sizes.map((size) => (
        <div key={size} className="flex flex-col items-center gap-1">
          <DatePicker {...context.args} size={size} />
          <Text variant="caption">{size}</Text>
        </div>
      ))}
    </div>
  ),
}
