import type { Meta, StoryObj } from '@storybook/react'
import { expect, userEvent, within } from '@storybook/test'
import { z } from 'zod'
import { Form } from '../Form'
import { Checkbox } from './Checkbox'

// Schema for our form
const defaultFormSchema = z.object({
  singleCheckbox: z.boolean(),
  multipleChoices: z.array(z.string()),
})

type Props = unknown

export default {
  title: 'Components/Checkbox',
  component: Checkbox,
  parameters: { layout: 'centered' },
  decorators: [
    (Story) => (
      <div className="w-[300px]">
        <Story />
      </div>
    ),
  ],
  argTypes: {
    isDisabled: {
      control: 'boolean',
      description: 'Whether the checkbox is disabled',
      defaultValue: false,
    },
    isReadOnly: {
      control: 'boolean',
      description: 'Whether the checkbox is read-only',
      defaultValue: false,
    },
    isInvalid: {
      control: 'boolean',
      description: 'Whether the checkbox has an invalid state',
      defaultValue: false,
    },
    isIndeterminate: {
      control: 'boolean',
      description: 'Whether the checkbox is in an indeterminate state',
      defaultValue: false,
    },
    children: {
      control: 'text',
      description: 'The label content for the checkbox',
    },
    name: {
      control: 'text',
      description: 'The name of the checkbox field',
    },
    value: {
      control: 'text',
      description: 'The value of the checkbox (required when used in a group)',
    },
  },
} satisfies Meta<Props>

type Story = StoryObj<Props>

export const Default: Story = {
  args: {
    name: 'singleCheckbox',
    children: 'Accept terms and conditions',
  },
  render: (args) => {
    return (
      <Form defaultValues={{ singleCheckbox: false }} schema={defaultFormSchema}>
        <Checkbox {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)

    const checkbox = canvas.getByRole('checkbox')

    await step('Checkbox should be initially unchecked', async () => {
      await expect(checkbox).not.toBeChecked()
    })

    await step('Checkbox can be checked', async () => {
      await userEvent.click(checkbox)
      await expect(checkbox).toBeChecked()
    })

    await step('Checkbox can be unchecked', async () => {
      await userEvent.click(checkbox)
      await expect(checkbox).not.toBeChecked()
    })
  },
}

export const Disabled: Story = {
  args: {
    name: 'singleCheckbox',
    children: 'Disabled checkbox',
    isDisabled: true,
  },
  render: (args) => {
    return (
      <Form defaultValues={{ singleCheckbox: false }} schema={defaultFormSchema}>
        <Checkbox {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkbox = canvas.getByRole('checkbox')

    await expect(checkbox).toBeDisabled()
    await expect(checkbox).not.toBeChecked()
  },
}

export const Selected: Story = {
  args: {
    name: 'singleCheckbox',
    children: 'Selected checkbox',
  },
  render: (args) => {
    return (
      <Form defaultValues={{ singleCheckbox: true }} schema={defaultFormSchema}>
        <Checkbox {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkbox = canvas.getByRole('checkbox')

    await expect(checkbox).toBeChecked()
  },
}

export const Invalid: Story = {
  args: {
    name: 'singleCheckbox',
    children: 'Invalid checkbox',
    isInvalid: true,
  },
  render: (args) => {
    return (
      <Form defaultValues={{ singleCheckbox: false }} schema={defaultFormSchema}>
        <Checkbox {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkbox = canvas.getByRole('checkbox')

    await expect(checkbox).toHaveAttribute('aria-invalid', 'true')
  },
}

export const ReadOnly: Story = {
  args: {
    name: 'singleCheckbox',
    children: 'Read-only checkbox',
    isReadOnly: true,
  },
  render: (args) => {
    return (
      <Form defaultValues={{ singleCheckbox: true }} schema={defaultFormSchema}>
        <Checkbox {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkbox = canvas.getByRole('checkbox')

    await expect(checkbox).toHaveAttribute('aria-readonly', 'true')
    await expect(checkbox).toBeChecked()
  },
}

export const Indeterminate: Story = {
  args: {
    name: 'singleCheckbox',
    children: 'Indeterminate checkbox',
    isIndeterminate: true,
  },
  render: (args) => {
    return (
      <Form defaultValues={{ singleCheckbox: false }} schema={defaultFormSchema}>
        <Checkbox {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkbox = canvas.getByRole('checkbox')

    await userEvent.click(checkbox)
  },
}

type GroupStory = StoryObj<unknown>

export const CheckboxGroup: GroupStory = {
  render: () => {
    return (
      <Form defaultValues={{ multipleChoices: ['option1'] }} schema={defaultFormSchema}>
        <Checkbox.Group name="multipleChoices" label="Select multiple options">
          <Checkbox value="option1">Option 1</Checkbox>
          <Checkbox value="option2">Option 2</Checkbox>
          <Checkbox value="option3">Option 3</Checkbox>
        </Checkbox.Group>
      </Form>
    )
  },
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)
    const checkboxes = canvas.getAllByRole('checkbox')

    await step('First checkbox should be initially checked', async () => {
      await expect(checkboxes[0]).toBeChecked()
      await expect(checkboxes[1]).not.toBeChecked()
      await expect(checkboxes[2]).not.toBeChecked()
    })

    await step('Can select multiple checkboxes', async () => {
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
      await userEvent.click(checkboxes[1]!)

      await expect(checkboxes[0]).toBeChecked()
      await expect(checkboxes[1]).toBeChecked()
      await expect(checkboxes[2]).not.toBeChecked()
    })
  },
}

export const DisabledGroup: GroupStory = {
  render: () => {
    return (
      <Form defaultValues={{ multipleChoices: ['option1'] }} schema={defaultFormSchema}>
        <Checkbox.Group name="multipleChoices" label="Disabled group" isDisabled>
          <Checkbox value="option1">Option 1</Checkbox>
          <Checkbox value="option2">Option 2</Checkbox>
          <Checkbox value="option3">Option 3</Checkbox>
        </Checkbox.Group>
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkboxes = canvas.getAllByRole('checkbox')

    for (const checkbox of checkboxes) {
      await expect(checkbox).toBeDisabled()
    }
  },
}

export const InvalidGroup: GroupStory = {
  render: () => {
    return (
      <Form defaultValues={{ multipleChoices: [] }} schema={defaultFormSchema}>
        <Checkbox.Group
          name="multipleChoices"
          label="Invalid group"
          isInvalid
          description="Please select at least one option"
        >
          <Checkbox value="option1">Option 1</Checkbox>
          <Checkbox value="option2">Option 2</Checkbox>
          <Checkbox value="option3">Option 3</Checkbox>
        </Checkbox.Group>
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkboxes = canvas.getAllByRole('checkbox')

    for (const checkbox of checkboxes) {
      await expect(checkbox).toHaveAttribute('aria-invalid', 'true')
    }
  },
}

export const InvalidGroupFromForm: GroupStory = {
  render: () => {
    return (
      <Form
        defaultValues={{ multipleChoices: ['option1'] }}
        schema={(schema) =>
          schema.object({
            multipleChoices: z
              .array(z.string())
              .min(1, { message: 'Please select at least one option' }),
          })
        }
      >
        <Checkbox.Group name="multipleChoices" label="Invalid group">
          <Checkbox value="option1">Option 1</Checkbox>
          <Checkbox value="option2">Option 2</Checkbox>
          <Checkbox value="option3">Option 3</Checkbox>
        </Checkbox.Group>
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const checkboxes = canvas.getAllByRole('checkbox')

    await expect(checkboxes[0]).toBeChecked()
    await expect(checkboxes[1]).not.toBeChecked()
    await expect(checkboxes[2]).not.toBeChecked()

    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    await userEvent.click(checkboxes[0]!)

    await expect(canvas.getByTestId('error')).toHaveTextContent('Please select at least one option')

    await expect(checkboxes[0]).not.toBeChecked()
    await expect(checkboxes[1]).not.toBeChecked()
    await expect(checkboxes[2]).not.toBeChecked()
  },
}
