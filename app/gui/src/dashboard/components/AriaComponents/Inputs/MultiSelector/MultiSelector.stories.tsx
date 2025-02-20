import type { Meta, StoryObj } from '@storybook/react'
import { userEvent, within } from '@storybook/test'
import { z } from 'zod'
import { Form } from '../../Form'
import type { MultiSelectorProps } from './MultiSelector.tsx'
import { MultiSelector } from './MultiSelector.tsx'

type Props = MultiSelectorProps<typeof schema, 'value', unknown>
type Story = StoryObj<Props>

const schema = z.object({ value: z.array(z.enum(['one', 'two', 'three'])) })

export default {
  title: 'Components/Inputs/MultiSelector',
  component: MultiSelector,
  render: (args) => <MultiSelector {...args} />,
  tags: ['autodocs'],
  decorators: [(Story, context) => <Form schema={schema}>{Story(context)}</Form>],
  args: { name: 'value', items: ['one', 'two', 'three'] },
} as Meta<Props>

export const Default: Story = {}

export const TwoColumns: Story = { args: { columns: 2 } }

export const SelectedItems: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    await userEvent.click(canvas.getByText('one'))

    await userEvent.click(canvas.getByText('two'))

    await userEvent.click(canvas.getByText('three'))

    await userEvent.click(canvas.getByText('one'))
  },
}
