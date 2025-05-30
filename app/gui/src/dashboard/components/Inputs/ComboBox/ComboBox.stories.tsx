import { roundedVariants } from '#/components/utilities'
import { StoryVariants } from '#/utilities/StoryVariants'
import type { Meta, StoryObj } from '@storybook/react'
import { z } from 'zod'
import { Form, type FieldPath, type TSchema } from '../../Form'
import type { ComboBoxProps } from './ComboBox'
import { ComboBox } from './ComboBox'

type Props = ComboBoxProps<typeof schema, 'value'>
type Story = StoryObj<Props>

const schema = z.object({ value: z.string() })

const sizes = ['medium', 'small'] as const
const roundeds = roundedVariants()

const args = {
  name: 'value',
  items: ['one', 'two', 'three'],
  className: 'w-40',
  children: (x: string) => x,
}

function ComboBoxWrapper<Schema extends TSchema, FieldName extends FieldPath<Schema, string>>(
  props: ComboBoxProps<Schema, FieldName>,
) {
  return <ComboBox {...props} />
}

export default {
  title: 'Components/Inputs/ComboBox',
  component: ComboBox,
  tags: ['autodocs'],
  decorators: [(Story, context) => <Form schema={schema}>{Story(context)}</Form>],
  args,
  parameters: {
    layout: 'centered',
  },
} as Meta<Props>

export const Default: Story = {}

export const Rounded: Story = {
  render: () => (
    <StoryVariants
      columns="3"
      render={ComboBoxWrapper}
      toProps={(rounded) => ({ ...args, rounded })}
      variants={roundeds}
    />
  ),
}

export const Size: Story = {
  render: () => (
    <StoryVariants
      render={ComboBoxWrapper}
      toProps={(size) => ({ ...args, size })}
      variants={sizes}
    />
  ),
}
