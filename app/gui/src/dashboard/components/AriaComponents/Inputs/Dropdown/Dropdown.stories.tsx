import type { Meta, StoryObj } from '@storybook/react'
import type { DropdownProps } from './Dropdown'
import { Dropdown } from './Dropdown'

type Props = DropdownProps<string>
type Story = StoryObj<Props>

export default {
  title: 'Components/Inputs/Dropdown',
  component: Dropdown,
  render: (args) => (
    <div>
      <Dropdown {...args} />
    </div>
  ),
  tags: ['autodocs'],
  args: { items: ['one', 'two', 'three'] },
} as Meta<Props>

export const Default: Story = { args: {} }
