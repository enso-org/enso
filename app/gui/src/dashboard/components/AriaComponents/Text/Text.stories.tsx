import type { Meta, StoryObj } from '@storybook/react'

import { omit } from 'enso-common/src/utilities/data/object'
import type { TextProps } from './Text'
import { Text } from './Text'

export default {
  title: 'Components/Text',
  component: Text,
  args: {
    children: 'Hello, world!',
  },
  parameters: {
    layout: 'centered',
  },
} as Meta<TextProps>

type Story = StoryObj<TextProps>

const variants = ['h1', 'subtitle', 'body', 'body-sm', 'caption', 'overline']
const weights = ['thin', 'normal', 'medium', 'semibold', 'bold', 'extraBold']

export const Variants: Story = {
  render: (args) => (
    <section className="flex flex-col gap-4">
      {variants.map((variant) => (
        <div key={variant} className="flex flex-col items-center gap-1">
          <Text {...args} variant={variant}>
            Lorem ipsum dolor sit amet.
          </Text>
          <Text variant="caption">{variant}</Text>
        </div>
      ))}
    </section>
  ),
}

const colorsProps = [
  { color: 'primary' },
  { color: 'danger' },
  { color: 'invert', className: 'bg-primary px-2 rounded-md' },
  { color: 'success' },
  { color: 'disabled' },
  { color: 'custom', className: 'text-youtube' },
] satisfies readonly Partial<TextProps>[]

export const Colors: Story = {
  render: (args) => (
    <section className="flex flex-col gap-4">
      {colorsProps.map((props) => (
        <div key={props.color} className="flex flex-col items-center gap-1">
          <Text {...args} {...props}>
            Lorem ipsum dolor sit amet.
          </Text>
          <Text variant="caption">{props.color}</Text>
        </div>
      ))}
    </section>
  ),
}

export const Weights: Story = {
  render: (args) => (
    <section className="flex flex-col gap-4">
      {weights.map((weight) => (
        <div key={weight} className="flex flex-col items-center gap-1">
          <Text {...args} weight={weight}>
            Lorem ipsum dolor sit amet.
          </Text>
          <Text variant="caption">{weight}</Text>
        </div>
      ))}
    </section>
  ),
}

const restProps = [
  { balance: true, className: 'block w-48' },
  { truncate: '1', className: 'block w-48' },
  { variant: 'h1', truncate: '2', className: 'w-48' },
  { truncate: 'custom', lineClamp: 2, className: 'w-48' },
] satisfies readonly Partial<TextProps>[]

function stringifyJSX(value: unknown) {
  if (typeof value !== 'object' || value == null) {
    return String(value)
  }
  return Object.entries(value)
    .flatMap(([k, v]) => {
      if (v === true) {
        return [`${k}`]
      }
      if (v === false) {
        return []
      }
      if (typeof v === 'string') {
        return [`${k}=${JSON.stringify(v)}`]
      }
      return [`${k}={${JSON.stringify(v)}}`]
    })
    .join(' ')
}

export const Rest: Story = {
  render: (args) => (
    <section className="flex flex-col gap-4">
      {restProps.map((props, i) => (
        <div key={i} className="flex flex-col items-center gap-1">
          <Text {...args} {...props}>
            Lorem ipsum dolor sit amet.
          </Text>
          <Text variant="caption">{stringifyJSX(omit(props, 'className'))}</Text>
        </div>
      ))}
    </section>
  ),
}
