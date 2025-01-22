import type { Meta, StoryObj } from '@storybook/react'
import { z } from 'zod'

import { Form } from '#/components/AriaComponents'
import { userEvent, within } from '@storybook/test'
import type { SelectorProps } from './Selector'
import { Selector } from './Selector'

// Schema for our form
const schema = z.object({
  plan: z.enum(['basic', 'pro', 'enterprise']),
})

type Props = SelectorProps<typeof schema, 'plan', 'basic' | 'enterprise' | 'pro'>

export default {
  title: 'Components/Inputs/Selector',
  component: Selector,
  parameters: {
    layout: 'centered',
  },
  render: (args) => <Selector {...args} />,
  args: {
    name: 'plan',
    items: ['basic', 'pro', 'enterprise'],
  },
  decorators: [
    (Story, context) => (
      <Form schema={schema} className="w-96" defaultValues={{ plan: 'basic' }}>
        {Story(context)}
      </Form>
    ),
  ],
} as Meta<Props>

type Story = StoryObj<Props>

// Basic usage
export const Default: Story = {}

// Different rounded variants
export const VisualVariants: Story = {
  render: (args) => {
    return (
      <div className="w-full space-y-12">
        <div className="w-full space-y-2">
          {(['outline'] as const).map((variant) => (
            <Selector {...args} key={variant} label={variant} variant={variant} />
          ))}
        </div>

        <div className="w-full space-y-2">
          {(['medium', 'small'] as const).map((size) => (
            <Selector {...args} key={size} label={size} size={size} />
          ))}
        </div>

        <div className="w-full space-y-2">
          {(
            ['medium', 'xxxlarge', 'none', 'small', 'large', 'xlarge', 'xxlarge', 'full'] as const
          ).map((rounded) => (
            <Selector {...args} key={rounded} label={rounded} rounded={rounded} />
          ))}
        </div>

        <div className="w-full space-y-2">
          <Selector {...args} label="Invalid" isInvalid />
          <Selector {...args} label="Required" isRequired />
          <Selector {...args} label="Disabled" isDisabled />
          <Selector {...args} label="Readonly" isReadOnly />
          <Selector {...args} label="Invalid & Disabled" isInvalid isDisabled />
        </div>
      </div>
    )
  },
}

export const Interactions: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)

    await userEvent.click(canvas.getByText('enterprise'))
  },
}
