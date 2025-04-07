import type { Meta, StoryObj } from '@storybook/react'
import { IconDisplay, type IconDisplayProps } from './IconDisplay'

type Props = IconDisplayProps<string>
type Story = StoryObj<Props>

export default {
  title: 'Components/IconDisplay',
  component: IconDisplay,
  render: (args) => <IconDisplay {...args} />,
  tags: ['autodocs'],
  args: { icon: 'time', children: 'aaaaaaaa' } satisfies Props,
  // `text-primary` is required to make icons show up.
  decorators: [(Story, context) => <div className="text-primary">{Story(context)}</div>],
  parameters: {
    layout: 'centered',
  },
} as Meta<Props>

export const Default: Story = {}

export const Overflowing: Story = {
  args: {
    icon: 'sort',
    children: 'very long example label that overflows the max width',
  } satisfies Props,
}
