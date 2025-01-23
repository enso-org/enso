import type { Meta, StoryObj } from '@storybook/react'

import type { TextProps } from './Text'
import { Text } from './Text'

export default {
  title: 'Components/Text',
  component: Text,
  args: {
    children: 'Hello, world!',
  },
} as Meta<TextProps>

type Story = StoryObj<TextProps>

export const Variants: Story = {
  render: (args) => (
    <section className="flex flex-col gap-4">
      <Text {...args} variant="h1">
        Lorem ipsum dolor sit amet h1.
      </Text>
      <Text {...args} variant="subtitle">
        Lorem ipsum dolor sit amet subtitle.
      </Text>
      <Text {...args} variant="body">
        Lorem ipsum dolor sit amet body.
      </Text>
      <Text {...args} variant="body-sm">
        Lorem ipsum dolor sit amet body-sm.
      </Text>
      <Text {...args} variant="caption">
        Lorem ipsum dolor sit amet caption.
      </Text>
      <Text {...args} variant="overline">
        Lorem ipsum dolor sit amet overline.
      </Text>
    </section>
  ),
}

export const Colors: Story = {
  render: (args) => (
    <section className="flex flex-col gap-4">
      <Text {...args} color="primary">
        Lorem ipsum dolor sit amet primary.
      </Text>
      <Text {...args} color="danger">
        Lorem ipsum dolor sit amet danger.
      </Text>
      <Text {...args} color="invert" className="bg-primary">
        Lorem ipsum dolor sit amet invert.
      </Text>
      <Text {...args} color="success">
        Lorem ipsum dolor sit amet success.
      </Text>
      <Text {...args} color="disabled">
        Lorem ipsum dolor sit amet disabled.
      </Text>
      <Text {...args} color="custom" className="text-youtube">
        Lorem ipsum dolor sit amet custom.
      </Text>
    </section>
  ),
}

export const Rest: Story = {
  render: (args) => (
    <>
      <Text {...args} balance className="block w-48">
        Lorem ipsum dolor sit amet slkdmflkasd Balance.
      </Text>

      <Text {...args} truncate="1" className="block w-48">
        Text truncate 1. Should display tooltip on hover.
      </Text>

      <Text {...args} variant="h1" truncate="2" className="w-48">
        Text truncate 2. Should display tooltip on hover. Does not work with custom display.
      </Text>

      <Text {...args} truncate="custom" lineClamp={2} className="w-48">
        Text truncate custom. Should display tooltip on hover. Does not work with custom display.
      </Text>

      <Text {...args} weight="thin" className="w-48">
        Text weight thin.
      </Text>

      <Text {...args} weight="normal" className="w-48">
        Text weight normal.
      </Text>

      <Text {...args} weight="medium" className="w-48">
        Text weight medium.
      </Text>

      <Text {...args} weight="semibold" className="w-48">
        Text weight semibold.
      </Text>

      <Text {...args} weight="bold" className="w-48">
        Text weight bold.
      </Text>

      <Text {...args} weight="extraBold" className="w-48">
        Text weight extraBold.
      </Text>
    </>
  ),
}
