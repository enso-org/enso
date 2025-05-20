import { Icon } from '#/components/Icon'
import { ICON_COLORS } from '#/components/Icon/Icon'
import { StoryVariants } from '#/utilities/StoryVariants'
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

const icon = <Icon icon="enso_logo" />

export const Hidden: Story = {
  render: () => (
    <StoryVariants
      render={StatusBadge}
      toProps={(props) => ({ color: 'danger' as const, children: icon, ...props })}
      variants={[{}, { hidden: true }]}
    />
  ),
}

export const Colors: Story = {
  render: () => (
    <StoryVariants
      render={StatusBadge}
      toProps={(color) => ({ color, children: icon })}
      variants={ICON_COLORS}
    />
  ),
}
