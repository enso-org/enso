import type { Meta, StoryObj } from '@storybook/react'
import { useState } from 'react'
import { Check } from './Check'

const meta = {
  title: 'Components/Check',
  component: Check,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof Check>

export default meta
type Story = StoryObj<typeof Check>

// Basic states
export const States: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <Check isSelected={false} />
      <Check isSelected={true} />
      <Check isSelected={true} isIndeterminate={true} />
      <Check isSelected={true} isPressed={true} />
      <Check isPressed />
    </div>
  ),
}

// Different sizes
export const Sizes: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <Check isSelected={true} size="small" />
      <Check isSelected={true} size="medium" />
      <Check isSelected={true} size="large" />
    </div>
  ),
}

// Different colors
export const Colors: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <Check isSelected={true} color="primary" />
      <Check isSelected={true} color="accent" />
      <Check isSelected={true} color="error" />
    </div>
  ),
}

// Different border radius options
export const BorderRadius: Story = {
  render: () => (
    <div className="flex items-center gap-4">
      <Check isSelected={true} rounded="none" />
      <Check isSelected={true} rounded="small" />
      <Check isSelected={true} rounded="medium" />
      <Check isSelected={true} rounded="large" />
      <Check isSelected={true} rounded="xlarge" />
      <Check isSelected={true} rounded="xxlarge" />
      <Check isSelected={true} rounded="xxxlarge" />
      <Check isSelected={true} rounded="full" />
    </div>
  ),
}

// Combined variations
export const CombinedVariations: Story = {
  render: () => (
    <div className="flex flex-col gap-4">
      {/* Row 1: Primary color variations */}
      <div className="flex items-center gap-4">
        <Check isSelected={true} color="primary" size="small" rounded="small" />
        <Check isSelected={true} color="primary" size="medium" rounded="medium" />
        <Check isSelected={true} color="primary" size="large" rounded="large" />
        <Check
          isSelected={true}
          color="primary"
          size="large"
          rounded="full"
          isIndeterminate={true}
        />
      </div>

      {/* Row 2: Accent color variations */}
      <div className="flex items-center gap-4">
        <Check isSelected={true} color="accent" size="small" rounded="small" />
        <Check isSelected={true} color="accent" size="medium" rounded="medium" />
        <Check isSelected={true} color="accent" size="large" rounded="large" />
        <Check
          isSelected={true}
          color="accent"
          size="large"
          rounded="full"
          isIndeterminate={true}
        />
      </div>

      {/* Row 3: Error color variations */}
      <div className="flex items-center gap-4">
        <Check isSelected={true} color="error" size="small" rounded="small" />
        <Check isSelected={true} color="error" size="medium" rounded="medium" />
        <Check isSelected={true} color="error" size="large" rounded="large" />
        <Check isSelected={true} color="error" size="large" rounded="full" isIndeterminate={true} />
      </div>
    </div>
  ),
}

// Interactive example
function InteractiveCheck() {
  const [isSelected, setIsSelected] = useState(false)
  const [isPressed, setIsPressed] = useState(false)

  return (
    <div
      className="cursor-pointer"
      onMouseDown={() => {
        setIsPressed(true)
      }}
      onMouseUp={() => {
        setIsPressed(false)
      }}
      onMouseLeave={() => {
        setIsPressed(false)
      }}
      onClick={() => {
        setIsSelected(!isSelected)
      }}
    >
      <Check isSelected={isSelected} isPressed={isPressed} size="large" color="primary" />
    </div>
  )
}

export const Interactive: Story = {
  render: () => <InteractiveCheck />,
}
