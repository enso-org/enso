import type { Meta, StoryObj } from '@storybook/react'
import { z } from 'zod'

import { type FieldPath, type TSchema } from '#/components/AriaComponents'
import { Form } from '#/components/AriaComponents/Form'
import { StoryVariants } from '#/utilities/StoryVariants'
import { userEvent, within } from '@storybook/test'
import type { SelectorProps } from './Selector'
import { Selector } from './Selector'

// Schema for our form
const schema = z.object({
  plan: z.enum(['basic', 'pro', 'enterprise']),
})

type Props = SelectorProps<typeof schema, 'plan', 'basic' | 'enterprise' | 'pro'>

const args = {
  name: 'plan',
  items: ['basic', 'pro', 'enterprise'],
}

function SelectorWrapper<Schema extends TSchema, FieldName extends FieldPath<Schema, T>, T>(
  props: SelectorProps<Schema, FieldName, T>,
) {
  return <Selector {...props} />
}

export default {
  title: 'Components/Inputs/Selector',
  component: Selector,
  parameters: {
    layout: 'centered',
  },
  args,
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

export const Variants: Story = {
  render: () => (
    <StoryVariants
      render={SelectorWrapper}
      toProps={(variant) => ({ ...args, variant })}
      variants={['outline']}
    />
  ),
}

export const Size: Story = {
  render: () => (
    <StoryVariants
      render={SelectorWrapper}
      toProps={(size) => ({ ...args, size })}
      variants={['medium', 'small']}
    />
  ),
}

export const Rounded: Story = {
  render: () => (
    <StoryVariants
      render={SelectorWrapper}
      toProps={(rounded) => ({ ...args, rounded })}
      variants={['medium', 'xxxlarge', 'none', 'small', 'large', 'xlarge', 'xxlarge', 'full']}
    />
  ),
}

export const State: Story = {
  render: () => (
    <StoryVariants
      render={SelectorWrapper}
      toProps={(props) => ({ ...args, ...props })}
      variants={[
        { isInvalid: true },
        { isRequired: true },
        { isDisabled: true },
        { isReadOnly: true },
        { isInvalid: true, isDisabled: true },
      ]}
    />
  ),
}

export const Interactions: Story = {
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)

    await userEvent.click(canvas.getByText('enterprise'))
  },
}
