import type { Meta, StoryObj } from '@storybook/react'
import { omit } from 'enso-common/src/utilities/data/object'
import { Text } from '../Text'
import { ProgressBar } from './ProgressBar'

const meta = {
  title: 'Components/ProgressBar',
  component: ProgressBar,
  decorators: [
    (Story, context) => {
      context = { ...context, args: { ...context.args } }
      if ('indeterminate' in context.args) {
        if (context.args.indeterminate === true) {
          context.args.progress = 'indeterminate'
        }
        delete context.args.indeterminate
      }
      return Story(context)
    },
  ],
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof ProgressBar>

export default meta
type Story = StoryObj<typeof ProgressBar>

export const Default: Story = {
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
    <div className="flex flex-col items-center gap-4">
      {[0, 0.2, 0.4, 0.6, 0.8, 1, 'indeterminate' as const].map((progress) => (
        <div key={progress} className="flex flex-col items-center gap-1">
          <ProgressBar progress={progress} className="h-2 w-40" />
          <Text variant="caption">{progress}</Text>
        </div>
      ))}
    </div>
  ),
}
