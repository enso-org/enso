import type { Meta, StoryObj } from '@storybook/react'
import { ProgressBar } from './ProgressBar'

const meta = {
  title: 'Components/ProgressBar',
  component: ProgressBar,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof ProgressBar>

export default meta
type Story = StoryObj<typeof ProgressBar>

export const Progress: Story = {
  render: () => (
    <div className="flex flex-col items-center gap-4">
      <ProgressBar progress={0} className="h-2 w-40" />
      <ProgressBar progress={0.2} className="h-2 w-40" />
      <ProgressBar progress={0.4} className="h-2 w-40" />
      <ProgressBar progress={0.6} className="h-2 w-40" />
      <ProgressBar progress={0.8} className="h-2 w-40" />
      <ProgressBar progress={1} className="h-2 w-40" />
      <ProgressBar progress="indeterminate" className="h-2 w-40" />
    </div>
  ),
}
