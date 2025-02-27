import { Text } from '#/components/AriaComponents/Text'
import { roundedVariants } from '#/components/AriaComponents/utilities'
import type { Meta, StoryObj } from '@storybook/react'
import { z } from 'zod'
import { Form } from '../../Form/index'
import type { ComboBoxProps } from './ComboBox'
import { ComboBox } from './ComboBox'

type Props = ComboBoxProps<typeof schema, 'value'>
type Story = StoryObj<Props>

const schema = z.object({ value: z.string() })

const sizes = ['medium', 'small'] as const
const roundeds = roundedVariants()

export default {
  title: 'Components/Inputs/ComboBox',
  component: ComboBox,
  render: (args) => <ComboBox {...args} />,
  tags: ['autodocs'],
  decorators: [(Story, context) => <Form schema={schema}>{Story(context)}</Form>],
  args: {
    name: 'value',
    items: ['one', 'two', 'three'],
    className: 'w-40',
    children: (x: string) => x,
  },
  parameters: {
    layout: 'centered',
  },
} as Meta<Props>

export const Default: Story = {}

export const Rounded: Story = {
  render: (_Story, context) => (
    <div className="grid grid-cols-3 gap-4">
      {roundeds.map((rounded) => (
        <div key={rounded} className="flex flex-col items-center gap-1">
          <ComboBox {...context.args} rounded={rounded} />
          <Text variant="caption">{rounded}</Text>
        </div>
      ))}
    </div>
  ),
}

export const Size: Story = {
  render: (_Story, context) => (
    <div className="grid grid-cols-1 gap-4">
      {sizes.map((size) => (
        <div key={size} className="flex flex-col items-center gap-1">
          <ComboBox {...context.args} size={size} />
          <Text variant="caption">{size}</Text>
        </div>
      ))}
    </div>
  ),
}
