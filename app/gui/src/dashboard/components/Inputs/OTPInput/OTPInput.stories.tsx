import type { Meta, StoryObj } from '@storybook/react'
import { expect, fn, userEvent, within } from '@storybook/test'
import { Form } from '../../Form'
import type { OtpInputProps } from './OTPInput'
import { OTPInput } from './OTPInput'

// Schema for our form
const defaultFormSchema = Form.schema.object({
  otp: Form.schema.string().min(6, 'OTP must be 6 digits'),
})

type Props = OtpInputProps<typeof defaultFormSchema, 'otp'>

export default {
  title: 'Components/Inputs/OTPInput',
  component: OTPInput,
  parameters: { layout: 'centered' },
  render: (args) => (
    <Form defaultValues={{ otp: '' }} schema={defaultFormSchema}>
      <OTPInput {...args} />
    </Form>
  ),
  decorators: [
    (Story) => (
      <div className="w-[400px]">
        <Story />
      </div>
    ),
  ],
  argTypes: {
    maxLength: {
      control: 'number',
      description: 'The maximum length of the OTP input',
      defaultValue: 6,
    },
    isDisabled: {
      control: 'boolean',
      description: 'Whether the OTP input is disabled',
      defaultValue: false,
    },
    isReadOnly: {
      control: 'boolean',
      description: 'Whether the OTP input is read-only',
      defaultValue: false,
    },
    isInvalid: {
      control: 'boolean',
      description: 'Whether the OTP input has an invalid state',
      defaultValue: false,
    },
    submitOnComplete: {
      control: 'boolean',
      description: 'Whether to submit the form when OTP is complete',
      defaultValue: true,
    },
  },
} satisfies Meta<Props>

type Story = StoryObj<Props>

export const Default: Story = {
  args: { name: 'otp', maxLength: 6 },
  render: (args) => {
    return (
      <Form defaultValues={{ otp: '' }} schema={defaultFormSchema}>
        <OTPInput {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement, step }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await step('Can input digits', async () => {
      await userEvent.type(input, '123456')
      await expect(input).toHaveValue('123456')
    })
  },
}

const SubmitOnCompleteFn = fn()

export const SubmitOnComplete: Story = {
  args: {
    name: 'otp',
    maxLength: 6,
    submitOnComplete: true,
  },
  render: (args) => (
    <Form defaultValues={{ otp: '' }} schema={defaultFormSchema} onSubmit={SubmitOnCompleteFn}>
      <OTPInput {...args} />
    </Form>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await userEvent.type(input, '123456')
    await expect(input).toHaveValue('123456')
    await expect(SubmitOnCompleteFn).toBeCalledWith({ otp: '123456' }, expect.anything())
  },
}

export const SubmitOnClick: Story = {
  args: {
    name: 'otp',
    maxLength: 6,
    submitOnComplete: false,
  },
  render: (args) => (
    <Form defaultValues={{ otp: '' }} schema={defaultFormSchema} onSubmit={SubmitOnCompleteFn}>
      <OTPInput {...args} />
      <Form.Submit>Submit</Form.Submit>
      <Form.FormError />
    </Form>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await userEvent.type(input, '666666')
    await userEvent.click(canvas.getByRole('button', { name: 'Submit' }))
    await expect(SubmitOnCompleteFn).toBeCalledWith({ otp: '666666' }, expect.anything())
  },
}

export const Disabled: Story = {
  args: {
    name: 'otp',
    maxLength: 6,
    isDisabled: true,
  },
  render: (args) => {
    return (
      <Form defaultValues={{ otp: '' }} schema={defaultFormSchema}>
        <OTPInput {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await expect(input).toBeDisabled()
    await expect(input).toHaveValue('')
  },
}

export const WithValidation: Story = {
  args: {
    name: 'otp',
    maxLength: 6,
  },
  render: (args) => {
    return (
      <Form
        defaultValues={{ otp: '' }}
        schema={(schema) =>
          schema.object({
            otp: schema.string().length(6, 'OTP must be exactly 6 digits'),
          })
        }
      >
        <OTPInput {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await userEvent.type(input, '12345')
    await userEvent.keyboard('{enter}')
    await expect(canvas.getByTestId('error')).toHaveTextContent('OTP must be exactly 6 digits')
  },
}

export const DifferentLength: Story = {
  args: {
    name: 'otp',
    maxLength: 4,
  },
  render: (args) => {
    return (
      <Form
        defaultValues={{ otp: '' }}
        schema={(schema) =>
          schema.object({
            otp: schema.string().length(4, 'OTP must be exactly 4 digits'),
          })
        }
      >
        <OTPInput {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await userEvent.type(input, '1234')
    await expect(input).toHaveValue('1234')
  },
}

export const WithInitialValue: Story = {
  args: {
    name: 'otp',
    maxLength: 6,
  },
  render: (args) => {
    return (
      <Form defaultValues={{ otp: '123456' }} schema={defaultFormSchema}>
        <OTPInput {...args} />
      </Form>
    )
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const input = canvas.getByRole('textbox')

    await expect(input).toHaveValue('123456')
  },
}
