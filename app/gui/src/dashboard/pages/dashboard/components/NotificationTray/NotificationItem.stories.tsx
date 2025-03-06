import { iconNames } from '@/util/iconName'
import type { Meta, StoryObj } from '@storybook/react'
import { NotificationItem, type NotificationItemProps } from './NotificationItem'

const meta = {
  title: 'Dashboard/Components/NotificationItem',
  component: NotificationItem,
  decorators: [(Story, context) => <div className="w-64">{Story(context)}</div>],
  args: {},
  argTypes: {
    icon: { options: iconNames, control: { type: 'select' } },
  },
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof NotificationItem>

export default meta
type Story = StoryObj<typeof NotificationItem>

export const Default: Story = {
  args: {
    icon: 'refresh',
    id: '',
    message: 'test notification content',
  } satisfies NotificationItemProps,
}

export const WithProgressBar: Story = {
  args: {
    icon: 'add',
    id: '',
    message: 'test notification content',
    progress: 0.25,
  } satisfies NotificationItemProps,
}
