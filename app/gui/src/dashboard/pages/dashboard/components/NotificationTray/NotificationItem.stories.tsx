import { ICON_COLORS } from '#/components/Icon/Icon'
import { iconNames } from '@/util/iconName'
import type { Meta, StoryObj } from '@storybook/react'
import { omit } from 'enso-common/src/utilities/data/object'
import { NotificationItem, type NotificationItemProps } from './NotificationItem'

const meta = {
  title: 'Dashboard/Components/NotificationItem',
  component: NotificationItem,
  decorators: [
    (Story, context) => {
      context = { ...context, args: { ...context.args } }
      if ('indeterminate' in context.args) {
        if (context.args.indeterminate === true) {
          context.args.progress = 'indeterminate'
        }
        delete context.args.indeterminate
      }
      if ('time' in context.args) {
        if (context.args.time != null) {
          context.args.timestamp = Number(new Date(context.args.time))
        }
        delete context.args.time
      }
      return <div className="w-64">{Story(context)}</div>
    },
  ],
  args: {},
  argTypes: {
    icon: { options: iconNames, control: { type: 'select' } },
    color: { options: ICON_COLORS, control: { type: 'select' } },
    indeterminate: {
      control: { type: 'boolean' },
    },
    progress: {
      control: { type: 'number', min: 0, max: 1, step: 0.1 },
    },
    time: {
      control: { type: 'date' },
    },
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
    progress: 0.3,
  } satisfies NotificationItemProps,
}
