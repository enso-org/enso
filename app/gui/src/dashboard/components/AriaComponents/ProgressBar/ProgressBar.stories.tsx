import { StoryVariants } from '#/utilities/StoryVariants'
import type { Meta, StoryObj } from '@storybook/react'
import { ProgressBar, type ProgressBarProps } from './ProgressBar'

const args = {
  className: 'h-2 w-40',
}

const meta = {
  title: 'Components/ProgressBar',
  component: ProgressBar,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof ProgressBar>

export default meta
type Story = StoryObj<typeof ProgressBar>

export const Default: Story = {
  render: (props: ProgressBarProps & { indeterminate?: true }) => (
    <ProgressBar {...props} progress={props.indeterminate ? 'indeterminate' : props.progress} />
  ),
  args: { className: 'h-2 w-40', indeterminate: false, progress: 0.2 },
  argTypes: {
    indeterminate: {
      control: { type: 'boolean' },
    },
    progress: {
      control: { type: 'number', min: 0, max: 1, step: 0.1 },
    },
  },
}

export const Progress: Story = {
  render: () => (
    <StoryVariants
      render={ProgressBar}
      toProps={(progress) => ({ ...args, progress })}
      variants={[0, 0.2, 0.4, 0.6, 0.8, 1, 'indeterminate']}
    />
  ),
}
