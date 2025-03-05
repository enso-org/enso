import type { Meta, StoryObj } from '@storybook/react'
import { IconDisplay, type IconDisplayProps } from './IconDisplay'

type Props = IconDisplayProps<string>
type Story = StoryObj<Props>

export default {
  title: 'Components/IconDisplay',
  component: IconDisplay,
  render: (args) => <IconDisplay {...args} />,
  tags: ['autodocs'],
  args: {},
  parameters: {
    layout: 'centered',
  },
} as Meta<Props>

export const Default: Story = {
  args: {
    icon: 'time',
    children: 'aaaaaaaa',
  } satisfies Props,
}

export const Overflowing: Story = {
  args: {
    icon: 'sort',
    children: 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa',
  } satisfies Props,
}
