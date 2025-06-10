import { ICON_COLORS } from '#/components/Icon/Icon'
import { iconNames } from '@/util/iconMetadata/iconName'
import type { Meta, StoryObj } from '@storybook/react'
import { NotificationItem, type NotificationItemProps } from './NotificationItem'

interface NotificationItemWrapperProps extends Omit<NotificationItemProps, 'timestamp'> {
  readonly time?: Date | number
  readonly indeterminate?: boolean
}

function NotificationItemWrapper(props: NotificationItemWrapperProps) {
  const { indeterminate = false, progress, time, ...rest } = props

  return (
    <div className="w-64">
      <NotificationItem
        {...rest}
        timestamp={time != null ? Number(new Date(time)) : undefined}
        progress={indeterminate === true ? 'indeterminate' : progress}
      />
    </div>
  )
}

const meta = {
  title: 'Dashboard/Components/NotificationItem',
  component: NotificationItem,
  render: (props) => <NotificationItemWrapper {...props} />,
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
