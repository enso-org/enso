import { Icon } from '#/components/Icon'
import type { Meta, StoryObj } from '@storybook/react'
import { StatusBadge } from './StatusBadge'

const meta = {
  title: 'Components/StatusBadge',
  component: StatusBadge,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof StatusBadge>

export default meta
type Story = StoryObj<typeof StatusBadge>

export const Hidden: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <StatusBadge color="danger">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge hidden color="danger">
        <Icon icon="enso_logo" />
      </StatusBadge>
    </div>
  ),
}

export const Colors: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <StatusBadge color="accent">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="current">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="custom">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="danger">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="disabled">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="inherit">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="invert">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="muted">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="primary">
        <Icon icon="enso_logo" />
      </StatusBadge>
      <StatusBadge color="success">
        <Icon icon="enso_logo" />
      </StatusBadge>
    </div>
  ),
}
