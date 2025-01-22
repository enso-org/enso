import TOPBAR_LINKS from '#/configurations/topbarLinks.json'
import type { Meta, StoryObj } from '@storybook/react'
import { TOPBAR_LINKS_SCHEMA, UserBarHelpSection, type UserBarHelpSectionProps } from '../UserBar'

export default {
  title: 'Layouts/UserBar',
  component: UserBarHelpSection,
  render: (args: UserBarHelpSectionProps) => {
    TOPBAR_LINKS_SCHEMA.parse({ items: args.items })
    return <UserBarHelpSection {...args} />
  },
  args: {
    items: TOPBAR_LINKS.items as UserBarHelpSectionProps['items'],
  },
  parameters: {
    layout: 'centered',
  },
} satisfies Meta<UserBarHelpSectionProps>

export const Default: StoryObj<UserBarHelpSectionProps> = {}

export const WithItems: StoryObj<UserBarHelpSectionProps> = {
  args: {
    items: [
      {
        name: 'signInShortcut',
        url: 'https://www.google.com',
      },
      {
        name: 'submit',
        url: 'https://www.google.com',
      },
      {
        name: 'docs',
        url: 'https://www.google.com',
      },
    ],
  },
}

export const WithMenu: StoryObj<UserBarHelpSectionProps> = {
  args: {
    items: [
      {
        name: 'docs',
        url: 'https://www.google.com',
        menu: [
          {
            name: 'community',
            url: 'https://www.google.com',
          },
          {
            name: 'enso101',
            url: 'https://www.google.com',
          },
          {
            name: 'help',
            url: 'https://www.google.com',
          },
        ],
      },
    ],
  },
}

export const StandaloneMenu: StoryObj<UserBarHelpSectionProps> = {
  args: {
    items: [
      {
        name: 'docs',
        menu: [
          {
            name: 'community',
            url: 'https://www.google.com',
          },
          {
            name: 'enso101',
            url: 'https://www.google.com',
          },
          {
            name: 'help',
            url: 'https://www.google.com',
          },
        ],
      },
    ],
  },
}
