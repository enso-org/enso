import type { Meta, StoryObj } from '@storybook/react'

import { StoryVariants } from '#/utilities/StoryVariants'
import { omit } from 'enso-common/src/utilities/data/object'
import type { TextProps } from './Text'
import { Text } from './Text'

const args = {
  children: 'Lorem ipsum dolor sit amet.',
}

export default {
  title: 'Components/Text',
  component: Text,
  args,
  parameters: {
    layout: 'centered',
  },
} as Meta<TextProps>

type Story = StoryObj<TextProps>

const variants = ['h1', 'subtitle', 'body', 'body-sm', 'caption', 'overline'] as const
const weights = ['thin', 'normal', 'medium', 'semibold', 'bold', 'extraBold'] as const

export const Variants: Story = {
  render: () => (
    <StoryVariants
      render={Text}
      toProps={(variant) => ({ ...args, variant })}
      variants={variants}
    />
  ),
}

export const Colors: Story = {
  render: () => (
    <StoryVariants
      render={Text}
      toProps={(props) => ({ ...args, ...props })}
      toLabel={({ color }) => color}
      variants={[
        { color: 'primary' },
        { color: 'danger' },
        { color: 'invert', className: 'bg-primary px-2 rounded-md' },
        { color: 'success' },
        { color: 'disabled' },
        { color: 'custom', className: 'text-youtube' },
      ]}
    />
  ),
}

export const Weights: Story = {
  render: () => (
    <StoryVariants render={Text} toProps={(weight) => ({ ...args, weight })} variants={weights} />
  ),
}

export const Rest: Story = {
  render: () => (
    <StoryVariants
      render={Text}
      toProps={(variant) => ({ ...args, ...variant })}
      toLabel={(variant) => omit(variant, 'className', 'children')}
      variants={[
        {
          balance: true,
          className: 'block w-48',
          children: 'Lorem ipsum dolor sit amet verylongword Balance.',
        },
        {
          truncate: '1',
          className: 'block w-48',
          children: 'Text truncate 1. Should display tooltip on hover.',
        },
        {
          variant: 'h1',
          truncate: '2',
          className: 'w-48',
          children:
            'Text truncate 2. Should display tooltip on hover. Does not work with custom display.',
        },
        {
          truncate: 'custom',
          lineClamp: 2,
          className: 'w-48',
          children:
            'Text truncate custom. Should display tooltip on hover. Does not work with custom display.',
        },
      ]}
    />
  ),
}
