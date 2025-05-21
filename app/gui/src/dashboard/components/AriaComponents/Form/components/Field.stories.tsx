/**
 * @file
 *
 * Field component stories
 */
import type { Meta, StoryObj } from '@storybook/react'
import * as z from 'zod'

import { fn } from '@storybook/test'
import { Button } from '../../Button'
import { Text } from '../../Text'
import { Form } from '../Form'
import { Field } from './Field'
const onSubmit = fn()

// Schema for our form examples
const schema = z.object({ name: z.string() })

const meta: Meta<typeof Field> = {
  title: 'Components/Form/Field',
  component: Field,
  render: (props) => (
    <Field {...props}>
      <div className="w-full min-w-64 rounded bg-background px-2 py-1">
        <Text>Slot content</Text>
      </div>
    </Field>
  ),
  decorators: [
    (Story) => (
      <Form schema={schema} onSubmit={onSubmit}>
        <Story />
      </Form>
    ),
  ],
  args: { name: 'name', label: 'Name' },
  parameters: {
    layout: 'centered',
  },
}

export default meta
type Story = StoryObj<typeof Field>

export const Default: Story = {}

export const WithContextualHelp: Story = {
  args: {
    label: 'Field with Help',
    isRequired: true,
    description: 'This field includes additional information via contextual help',
    contextualHelp: (
      <>
        <Text variant="h1" color="primary">
          Help Information
        </Text>

        <Text variant="body" className="mt-2">
          This field requires specific formatting.
        </Text>

        <ul className="mt-1 list-disc pl-5">
          <li className="list-item items-center gap-1">
            <Text className="block">Must contain at least 3 characters</Text>
          </li>
          <li className="list-item items-center gap-1">
            <Text className="block">Cannot contain special characters</Text>
          </li>
          <li className="list-item items-center gap-1">
            <Text className="block">Will be displayed publicly</Text>
          </li>
        </ul>

        <Button icon="open_count" iconPosition="end" variant="ghost">
          Learn more
        </Button>
      </>
    ),
  },
}

export const WithError: Story = {
  args: {
    label: 'Field with Error',
    error: 'This field is required',
  },
}
