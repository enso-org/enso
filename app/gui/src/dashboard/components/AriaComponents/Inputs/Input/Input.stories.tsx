/** @file Stories for the Input component */
import { Form, Text } from '#/components/AriaComponents'
import type { Meta, StoryObj } from '@storybook/react'

import { fn } from '@storybook/test'
import { Input } from './Input'

const onSubmit = fn()

const meta: Meta<typeof Input> = {
  title: 'Components/Inputs/Input',
  component: Input,
  decorators: [
    (Story) => (
      <Form schema={(z) => z.object({ value: z.string().min(1) })} onSubmit={onSubmit}>
        <Story />
      </Form>
    ),
  ],
  args: {
    name: 'value',
  },
  parameters: {
    layout: 'centered',
  },
  argTypes: {
    variant: {
      control: 'select',
      options: ['primary', 'secondary', 'ghost'],
    },
    size: {
      control: 'select',
      options: ['small', 'medium', 'large'],
    },
    rounded: {
      control: 'select',
      options: ['none', 'small', 'medium', 'large', 'full'],
    },
    type: {
      control: 'select',
      options: ['text', 'password', 'email', 'number', 'date', 'tel', 'url'],
    },
    autoFocus: {
      control: 'radio',
      options: [false, true, 'select'],
    },
  },
}

export default meta

type Story = StoryObj<typeof Input>

export const Default: Story = {
  args: {
    placeholder: 'Enter text',
  },
}

export const WithLabel: Story = {
  args: {
    label: 'Username',
    placeholder: 'Enter username',
  },
}

export const WithDescription: Story = {
  args: {
    label: 'Email',
    description: 'We will never share your email with anyone else.',
    placeholder: 'Enter email',
  },
}

export const WithIcon: Story = {
  args: {
    label: 'Search',
    placeholder: 'Search for something...',
    icon: 'search', // Assumes a search icon is available
  },
}

export const WithAddonStart: Story = {
  args: {
    label: 'Price',
    placeholder: '0.00',
    addonStart: '$',
  },
}

export const WithAddonEnd: Story = {
  args: {
    label: 'Website',
    placeholder: 'example',
    addonEnd: '.com',
  },
}

export const WithBothAddons: Story = {
  args: {
    label: 'Price Range',
    placeholder: '0.00',
    addonStart: '$',
    addonEnd: 'USD',
  },
}

export const Disabled: Story = {
  args: {
    label: 'Disabled Input',
    placeholder: 'This input is disabled',
    isDisabled: true,
    value: 'Disabled content',
  },
}

export const Invalid: Story = {
  args: {
    label: 'Username',
    placeholder: 'Enter username',
    isInvalid: true,
    errorMessage: 'Username is required',
  },
}

export const Password: Story = {
  args: {
    label: 'Password',
    placeholder: 'Enter password',
    type: 'password',
  },
}

export const Number: Story = {
  args: {
    label: 'Quantity',
    placeholder: 'Enter quantity',
    type: 'number',
  },
}

export const Small: Story = {
  args: {
    label: 'Small Input',
    placeholder: 'Small size',
    size: 'small',
  },
}

export const Medium: Story = {
  args: {
    label: 'Medium Input',
    placeholder: 'Medium size',
    size: 'medium',
  },
}

export const Large: Story = {
  args: {
    label: 'Large Input',
    placeholder: 'Large size',
    size: 'large',
  },
}

export const Primary: Story = {
  args: {
    label: 'Primary Input',
    placeholder: 'Primary variant',
    variant: 'primary',
  },
}

export const Secondary: Story = {
  args: {
    label: 'Secondary Input',
    placeholder: 'Secondary variant',
    variant: 'secondary',
  },
}

export const Ghost: Story = {
  args: {
    label: 'Ghost Input',
    placeholder: 'Ghost variant',
    variant: 'ghost',
  },
}

export const RoundedNone: Story = {
  args: {
    label: 'No Rounding',
    placeholder: 'No rounded corners',
    rounded: 'none',
  },
}

export const RoundedSmall: Story = {
  args: {
    label: 'Small Rounding',
    placeholder: 'Small rounded corners',
    rounded: 'small',
  },
}

export const RoundedMedium: Story = {
  args: {
    label: 'Medium Rounding',
    placeholder: 'Medium rounded corners',
    rounded: 'medium',
  },
}

export const RoundedLarge: Story = {
  args: {
    label: 'Large Rounding',
    placeholder: 'Large rounded corners',
    rounded: 'large',
  },
}

export const RoundedFull: Story = {
  args: {
    label: 'Full Rounding',
    placeholder: 'Fully rounded corners',
    rounded: 'full',
  },
}

export const WithAutoFocus: Story = {
  args: {
    label: 'Auto Focus',
    placeholder: 'This input is auto focused',
    autoFocus: true,
  },
}

export const WithAutoSelect: Story = {
  args: {
    label: 'Auto Select',
    placeholder: 'This input is auto focused and text selected',
    autoFocus: 'select',
    value: 'Selected text',
  },
}

export const WithContextualHelp: Story = {
  args: {
    label: 'Contextual Help',
    placeholder: 'This input has contextual help',
    contextualHelp: <Text>This is contextual help</Text>,
  },
}
