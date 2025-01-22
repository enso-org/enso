/**
 * @file Stories for the Breadcrumbs component.
 */

import { Button } from '#/components/AriaComponents'
import type { Meta, StoryObj } from '@storybook/react'
import { Breadcrumbs } from '.'

export default {
  title: 'Components/Breadcrumbs',
  component: Breadcrumbs,
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<typeof Breadcrumbs>

type Story = StoryObj<typeof Breadcrumbs>

export const Default: Story = {
  render: () => (
    <Breadcrumbs>
      <Breadcrumbs.Item onPress={() => {}}>Home</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}}>Projects</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}} isDisabled>
        Current Project
      </Breadcrumbs.Item>
    </Breadcrumbs>
  ),
}

export const WithDisabledItem: Story = {
  render: () => (
    <Breadcrumbs>
      <Breadcrumbs.Item onPress={() => {}}>Home</Breadcrumbs.Item>
      <Breadcrumbs.Item isDisabled>Projects</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}} isDisabled>
        Current Project
      </Breadcrumbs.Item>
    </Breadcrumbs>
  ),
}

export const WithSuffix: Story = {
  render: () => (
    <Breadcrumbs>
      <Breadcrumbs.Item onPress={() => {}}>Home</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}}>Projects</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}} suffix={<Button variant="primary">Edit</Button>}>
        Current Project
      </Breadcrumbs.Item>
    </Breadcrumbs>
  ),
}

export const WithManyItems: Story = {
  render: () => (
    <div style={{ width: '500px' }}>
      <Breadcrumbs>
        <Breadcrumbs.Item onPress={() => {}}>Home</Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}}>Projects</Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}}>Team</Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}}>Documents</Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}}>Reports</Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}} isDisabled>
          Current Report
        </Breadcrumbs.Item>
      </Breadcrumbs>
    </div>
  ),
}

export const WithCustomSeparator: Story = {
  render: () => (
    <Breadcrumbs>
      <Breadcrumbs.Item onPress={() => {}}>Home</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}}>Projects</Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}} isDisabled>
        Current Project
      </Breadcrumbs.Item>
    </Breadcrumbs>
  ),
}

export const SingleItem: Story = {
  render: () => (
    <Breadcrumbs>
      <Breadcrumbs.Item onPress={() => {}} isDisabled>
        Home
      </Breadcrumbs.Item>
    </Breadcrumbs>
  ),
}
