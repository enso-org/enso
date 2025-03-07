import { StoryVariants } from '#/utilities/StoryVariants'
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
    <StoryVariants
      render={Check}
      variants={[
        { isSelected: false },
        { isSelected: true },
        { isSelected: true, isIndeterminate: true },
        { isSelected: true, isPressed: true },
        { isPressed: true },
      ]}
    />
  ),
}

// Different sizes
export const Sizes: Story = {
  render: () => (
    <StoryVariants
      render={Check}
      toProps={(size) => ({ isSelected: true, size })}
      variants={['small', 'medium', 'large']}
    />
  ),
}

// Different colors
export const Colors: Story = {
  render: () => (
    <StoryVariants
      render={Check}
      toProps={(color) => ({ isSelected: true, color })}
      variants={['primary', 'accent', 'error']}
    />
  ),
}

// Different border radius options
export const BorderRadius: Story = {
  render: () => (
    <StoryVariants
      render={Check}
      toProps={(rounded) => ({ isSelected: true, rounded })}
      variants={['none', 'small', 'medium', 'large', 'xlarge', 'xxlarge', 'xxxlarge', 'full']}
    />
  ),
}

// Combined variations
export const CombinedVariations: Story = {
  render: () => (
    <StoryVariants
      columns="4"
      render={Check}
      toProps={(props) => ({ isSelected: true, ...props })}
      variants={[
        // Row 1: Primary color variations
        { color: 'primary', size: 'small', rounded: 'small' },
        { color: 'primary', size: 'medium', rounded: 'medium' },
        { color: 'primary', size: 'large', rounded: 'large' },
        { color: 'primary', size: 'large', rounded: 'full', isIndeterminate: true },

        // Row 2: Accent color variations
        { color: 'accent', size: 'small', rounded: 'small' },
        { color: 'accent', size: 'medium', rounded: 'medium' },
        { color: 'accent', size: 'large', rounded: 'large' },
        { color: 'accent', size: 'large', rounded: 'full', isIndeterminate: true },

        // Row 3: Error color variations
        { color: 'error', size: 'small', rounded: 'small' },
        { color: 'error', size: 'medium', rounded: 'medium' },
        { color: 'error', size: 'large', rounded: 'large' },
        { color: 'error', size: 'large', rounded: 'full', isIndeterminate: true },
      ]}
    />
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
