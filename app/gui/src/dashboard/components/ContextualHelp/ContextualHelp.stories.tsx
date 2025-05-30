import type { Meta, StoryObj } from '@storybook/react'
import { useState } from 'react'
import { Button } from '../Button'
import { Text } from '../Text'
import { ContextualHelp } from './ContextualHelp'

const meta = {
  title: 'Components/ContextualHelp',
  component: ContextualHelp,
  parameters: {
    layout: 'centered',
  },
  argTypes: {
    placement: {
      control: 'select',
      options: [
        'top',
        'bottom',
        'left',
        'right',
        'top start',
        'top end',
        'bottom start',
        'bottom end',
        'left top',
        'left bottom',
        'right top',
        'right bottom',
      ],
    },
    variant: {
      control: 'radio',
      options: ['help', 'info'],
    },
  },
} satisfies Meta<typeof ContextualHelp>

export default meta
type Story = StoryObj<typeof meta>

export const Default: Story = {
  args: {
    placement: 'top',
    children: (
      <div className="max-w-xs p-4">
        <Text.Heading level={3} className="mb-2">
          Help Title
        </Text.Heading>
        <Text>
          This is a contextual help message that appears when the user hovers over the help icon.
        </Text>
      </div>
    ),
  },
}

export const Variants: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <div className="flex flex-col items-center gap-2">
        <Text variant="caption">Help Variant</Text>
        <ContextualHelp placement="top">
          <div className="max-w-xs p-4">
            <Text.Heading level={3} className="mb-2">
              Help
            </Text.Heading>
            <Text>This is the default help variant with a question mark icon.</Text>
          </div>
        </ContextualHelp>
      </div>

      <div className="flex flex-col items-center gap-2">
        <Text variant="caption">Info Variant</Text>
        <ContextualHelp variant="info" placement="top">
          <div className="max-w-xs p-4">
            <Text.Heading level={3} className="mb-2">
              Information
            </Text.Heading>
            <Text>This is the info variant with an information icon.</Text>
          </div>
        </ContextualHelp>
      </div>
    </div>
  ),
}

export const Placements: Story = {
  render: () => {
    const content = (
      <div className="max-w-xs p-4">
        <Text.Heading level={3} className="mb-2">
          Placement Demo
        </Text.Heading>
        <Text>This popover demonstrates different placement options.</Text>
      </div>
    )

    return (
      <div className="grid grid-cols-3 gap-8 p-8">
        <div className="flex justify-center">
          <ContextualHelp placement="top start">{content}</ContextualHelp>
        </div>
        <div className="flex justify-center">
          <ContextualHelp placement="top">{content}</ContextualHelp>
        </div>
        <div className="flex justify-center">
          <ContextualHelp placement="top end">{content}</ContextualHelp>
        </div>

        <div className="flex justify-end">
          <ContextualHelp placement="left top">{content}</ContextualHelp>
        </div>
        <div className="flex items-center justify-center">
          <div className="rounded border border-gray-200 p-6 text-center text-sm font-medium">
            Placements
          </div>
        </div>
        <div className="flex justify-start">
          <ContextualHelp placement="right top">{content}</ContextualHelp>
        </div>

        <div className="flex justify-center">
          <ContextualHelp placement="bottom start">{content}</ContextualHelp>
        </div>
        <div className="flex justify-center">
          <ContextualHelp placement="bottom">{content}</ContextualHelp>
        </div>
        <div className="flex justify-center">
          <ContextualHelp placement="bottom end">{content}</ContextualHelp>
        </div>
      </div>
    )
  },
}

export const ControlledState: Story = {
  render: () => {
    const [isOpen, setIsOpen] = useState(false)

    return (
      <div className="flex flex-col items-center gap-4">
        <Text>Controlled popup state: {isOpen ? 'Open' : 'Closed'}</Text>

        <div className="flex items-center gap-4">
          <Button
            onPress={() => {
              setIsOpen(!isOpen)
            }}
          >
            Toggle Help
          </Button>

          <ContextualHelp isOpen={isOpen} onOpenChange={setIsOpen} placement="right">
            <div className="max-w-xs p-4">
              <Text.Heading level={3} className="mb-2">
                Controlled Help
              </Text.Heading>
              <Text>This help popup is controlled by external state.</Text>
              <Button
                onPress={() => {
                  setIsOpen(false)
                }}
                className="mt-3"
              >
                Close
              </Button>
            </div>
          </ContextualHelp>
        </div>
      </div>
    )
  },
}

export const ComplexContent: Story = {
  render: () => (
    <div className="flex items-center justify-center">
      <ContextualHelp placement="bottom" variant="info">
        <div className="max-w-md p-4">
          <Text.Heading level={3} className="mb-2">
            Advanced Configuration
          </Text.Heading>

          <div className="mb-3">
            <Text>This demonstrates complex content within the help popover:</Text>
          </div>

          <div className="mb-3 rounded bg-gray-100 p-3">
            <Text variant="body-sm">
              {`// Example code\nconst value = calculateValue();\nreturn <Component value={value} />;`}
            </Text>
          </div>

          <ul className="mb-3 list-inside list-disc space-y-1">
            <li>
              <Text>Configure your settings</Text>
            </li>
            <li>
              <Text>Apply necessary changes</Text>
            </li>
            <li>
              <Text>Save your configuration</Text>
            </li>
          </ul>

          <Button variant="outline" className="mt-2">
            Learn More
          </Button>
        </div>
      </ContextualHelp>
    </div>
  ),
}
