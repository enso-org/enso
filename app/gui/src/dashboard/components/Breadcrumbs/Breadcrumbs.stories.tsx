/**
 * @file Stories for the Breadcrumbs component.
 */

import Cloud from '#/assets/cloud.svg'
import ArrowDown from '#/assets/expand_arrow.svg'
import Folder from '#/assets/folder.svg'
import Add from '#/assets/plus.svg'
import { Button, Menu } from '#/components/AriaComponents'
import type { Meta, StoryObj } from '@storybook/react'
import { expect, userEvent, within } from '@storybook/test'
import { useState } from 'react'
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
      <Breadcrumbs.Item onPress={() => {}}>
        Projects Lorem ipsum dolor sit amet consectetur adipisicing elit. Praesentium at voluptates
        necessitatibus ut quas quae veritatis optio natus? Sint voluptatem dolores, velit architecto
        ipsam officiis fugit eligendi. Molestiae, modi possimus.
      </Breadcrumbs.Item>
      <Breadcrumbs.Item onPress={() => {}} isCurrent>
        Very long title Lorem ipsum dolor sit amet consectetur, adipisicing elit. Quaerat quis
        aperiam libero dolorem molestias autem voluptas eum culpa ea ipsum suscipit modi asperiores
        optio perferendis accusamus dignissimos, assumenda sequi numquam?
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
      <Breadcrumbs.Item>Home</Breadcrumbs.Item>
      <Breadcrumbs.Item
        isDisabled
        addonEnd={
          <Menu.Trigger>
            <Button icon={ArrowDown} aria-label="Edit" />

            <Menu>
              <Menu.Item>Edit</Menu.Item>
              <Menu.Item>Delete</Menu.Item>
              <Menu.Item>Duplicate</Menu.Item>
              <Menu.Item>Rename</Menu.Item>
            </Menu>
          </Menu.Trigger>
        }
      >
        Projects
      </Breadcrumbs.Item>
      <Breadcrumbs.Item
        isCurrent
        addonEnd={
          <Menu.Trigger>
            <Button icon={ArrowDown} aria-label="Edit" />

            <Menu>
              <Menu.Item>Edit</Menu.Item>
              <Menu.Item>Delete</Menu.Item>
              <Menu.Item>Duplicate</Menu.Item>
              <Menu.Item>Rename</Menu.Item>
            </Menu>
          </Menu.Trigger>
        }
      >
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
        <Breadcrumbs.Item onPress={() => {}} href="https://google.com">
          Documents
        </Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}}>Reports</Breadcrumbs.Item>
        <Breadcrumbs.Item onPress={() => {}} isDisabled>
          Current Report
        </Breadcrumbs.Item>
      </Breadcrumbs>
    </div>
  ),

  play: async ({ canvasElement }) => {
    const { getByLabelText, findAllByRole } = within(canvasElement)
    await userEvent.click(getByLabelText('More'))

    const menuItems = await findAllByRole('menuitem')
    await expect(menuItems).toHaveLength(3)
  },
}

export const SingleItem: Story = {
  render: () => (
    <Breadcrumbs>
      <Breadcrumbs.Item onPress={() => {}}>Home</Breadcrumbs.Item>
    </Breadcrumbs>
  ),
}

export const Dynamic: Story = {
  render: () => {
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const [items, setItems] = useState<
      {
        id: number
        name: string
        href: string
        isCurrent: boolean
      }[]
    >([
      {
        id: 1,
        name: 'Home',
        href: `https://google.com/0`,
        isCurrent: true,
      },
    ])

    function addItem() {
      const nextItem = {
        id: items.length + 1,
        name: `Item ${items.length + 1}`,
        href: `https://google.com/${items.length + 1}`,
        isCurrent: true,
      }
      setItems([...items.map((item) => ({ ...item, isCurrent: false })), nextItem])
    }

    return (
      <Breadcrumbs items={items}>
        {(item) => (
          <Breadcrumbs.Item
            id={item.id}
            href={item.href}
            icon={Folder}
            isCurrent={item.isCurrent}
            addonEnd={
              item.isCurrent ? <Button icon={Add} aria-label="Add" onPress={addItem} /> : null
            }
          >
            {item.name}
          </Breadcrumbs.Item>
        )}
      </Breadcrumbs>
    )
  },

  play: async ({ canvasElement, step }) => {
    const { findAllByRole, getByLabelText } = within(canvasElement)
    function getLastItem() {
      const item = getByLabelText('Add')
      return item
    }

    async function addItems(count: number) {
      for (let i = 0; i < count; i++) {
        const lastItem = getLastItem()
        await userEvent.click(lastItem)
      }
    }

    await step('Click on the last item', async () => {
      await addItems(1)
    })

    await step('Check if the new item is added', async () => {
      const items = await findAllByRole('listitem')
      await expect(items).toHaveLength(2)
    })

    await step('add 3 more items', async () => {
      await addItems(3)
    })

    await step('Check if the new items are added', async () => {
      const items = await findAllByRole('listitem')
      await expect(items).toHaveLength(3)
      await expect(getByLabelText('More')).toBeInTheDocument()

      await userEvent.click(getByLabelText('More'))
      const menuItems = await findAllByRole('menuitem')
      await expect(menuItems).toHaveLength(2)
    })
  },
}

export const WithStartAddon: Story = {
  render: () => (
    <div className="space-y-4">
      {/* Basic icons as start addons */}
      <Breadcrumbs>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={<span className="text-gray-500">🏠</span>}
        >
          Home
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={<span className="text-gray-500">📁</span>}
        >
          Projects
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={<span className="text-gray-500">📄</span>}
          isCurrent
        >
          Current Project
        </Breadcrumbs.Item>
      </Breadcrumbs>

      {/* Status indicators as start addons */}
      <Breadcrumbs>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={<div className="bg-green-500 h-2 w-2 rounded-full" />}
        >
          Active Projects
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={<div className="h-2 w-2 rounded-full bg-yellow-500" />}
        >
          In Progress
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={<div className="h-2 w-2 rounded-full bg-blue-500" />}
          isCurrent
        >
          Project Details
        </Breadcrumbs.Item>
      </Breadcrumbs>

      {/* Buttons as start addons */}
      <Breadcrumbs>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={
            <Button
              variant="icon"
              onPress={() => {
                // Handle click
              }}
              aria-label="Refresh"
            >
              ↻
            </Button>
          }
        >
          Dashboard
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={
            <Button
              variant="icon"
              onPress={() => {
                // Handle click
              }}
              aria-label="Settings"
            >
              ⚙️
            </Button>
          }
        >
          Settings
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          addonStart={
            <Button variant="icon" aria-label="Edit">
              ✏️
            </Button>
          }
          isCurrent
        >
          Edit Profile
        </Breadcrumbs.Item>
      </Breadcrumbs>

      {/* Mixed content as start addons */}
      <Breadcrumbs>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={
            <div className="flex items-center gap-1">
              <span className="text-gray-500">👥</span>
              <span className="text-xs text-gray-500">5</span>
            </div>
          }
        >
          Team
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={
            <div className="flex items-center gap-1">
              <span className="text-gray-500">📊</span>
              <div className="h-1.5 w-8 rounded bg-gray-200">
                <div className="h-full w-3/4 rounded bg-blue-500" />
              </div>
            </div>
          }
        >
          Progress
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          addonStart={
            <div className="flex items-center gap-1">
              <span className="text-gray-500">🎯</span>
              <span className="text-xs text-gray-500">3/5</span>
            </div>
          }
          isCurrent
        >
          Milestones
        </Breadcrumbs.Item>
      </Breadcrumbs>
    </div>
  ),
}

export const WithCustomIcon: Story = {
  render: () => (
    <div className="space-y-4">
      {/* Simple SVG icons */}
      <Breadcrumbs>
        <Breadcrumbs.Item icon={Cloud}>Home</Breadcrumbs.Item>
        <Breadcrumbs.Item icon={Folder}>Projects</Breadcrumbs.Item>
        <Breadcrumbs.Item icon={Folder}>Team</Breadcrumbs.Item>
        <Breadcrumbs.Item icon={Folder}>Documents</Breadcrumbs.Item>
        <Breadcrumbs.Item icon={Folder}>Reports</Breadcrumbs.Item>
        <Breadcrumbs.Item icon={Folder} isCurrent>
          March 2025
        </Breadcrumbs.Item>
      </Breadcrumbs>

      {/* Icons with dynamic colors */}
      <Breadcrumbs>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          icon={({ isDisabled }) => (
            <svg
              className={`h-4 w-4 ${isDisabled ? 'text-gray-300' : 'text-blue-500'}`}
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z"
              />
            </svg>
          )}
        >
          Profile
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          icon={({ isDisabled }) => (
            <svg
              className={`h-4 w-4 ${isDisabled ? 'text-gray-300' : 'text-green-500'}`}
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
              />
            </svg>
          )}
        >
          Analytics
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          icon={({ isCurrent }) => (
            <svg
              className={`h-4 w-4 ${isCurrent ? 'text-indigo-600' : 'text-gray-500'}`}
              fill="none"
              stroke="currentColor"
              viewBox="0 0 24 24"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M10.325 4.317c.426-1.756 2.924-1.756 3.35 0a1.724 1.724 0 002.573 1.066c1.543-.94 3.31.826 2.37 2.37a1.724 1.724 0 001.065 2.572c1.756.426 1.756 2.924 0 3.35a1.724 1.724 0 00-1.066 2.573c.94 1.543-.826 3.31-2.37 2.37a1.724 1.724 0 00-2.572 1.065c-.426 1.756-2.924 1.756-3.35 0a1.724 1.724 0 00-2.573-1.066c-1.543.94-3.31-.826-2.37-2.37a1.724 1.724 0 00-1.065-2.572c-1.756-.426-1.756-2.924 0-3.35a1.724 1.724 0 001.066-2.573c-.94-1.543.826-3.31 2.37-2.37.996.608 2.296.07 2.572-1.065z"
              />
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M15 12a3 3 0 11-6 0 3 3 0 016 0z"
              />
            </svg>
          )}
          isCurrent
        >
          Settings
        </Breadcrumbs.Item>
      </Breadcrumbs>

      {/* Icons with badges */}
      <Breadcrumbs>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          icon={() => (
            <div className="relative">
              <svg
                className="h-4 w-4 text-gray-500"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M3 8l7.89 5.26a2 2 0 002.22 0L21 8M5 19h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v10a2 2 0 002 2z"
                />
              </svg>
              <span className="absolute -right-1 -top-1 flex h-2 w-2">
                <span className="absolute inline-flex h-full w-full animate-ping rounded-full bg-red-400 opacity-75" />
                <span className="relative inline-flex h-2 w-2 rounded-full bg-red-500" />
              </span>
            </div>
          )}
        >
          Messages
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          icon={() => (
            <div className="relative">
              <svg
                className="h-4 w-4 text-gray-500"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M15 17h5l-1.405-1.405A2.032 2.032 0 0118 14.158V11a6.002 6.002 0 00-4-5.659V5a2 2 0 10-4 0v.341C7.67 6.165 6 8.388 6 11v3.159c0 .538-.214 1.055-.595 1.436L4 17h5m6 0v1a3 3 0 11-6 0v-1m6 0H9"
                />
              </svg>
              <span className="absolute -right-1 -top-1 inline-flex h-3 w-3 items-center justify-center rounded-full bg-blue-500 text-[8px] text-white">
                3
              </span>
            </div>
          )}
        >
          Notifications
        </Breadcrumbs.Item>
        <Breadcrumbs.Item
          onPress={() => {
            // Handle click
          }}
          icon={() => (
            <div className="relative">
              <svg
                className="h-4 w-4 text-gray-500"
                fill="none"
                stroke="currentColor"
                viewBox="0 0 24 24"
              >
                <path
                  strokeLinecap="round"
                  strokeLinejoin="round"
                  strokeWidth={2}
                  d="M12 8v4l3 3m6-3a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
              <span className="absolute -right-1 -top-1 inline-flex h-2 w-2 rounded-full bg-yellow-400 ring-2 ring-white" />
            </div>
          )}
          isCurrent
        >
          Pending
        </Breadcrumbs.Item>
      </Breadcrumbs>
    </div>
  ),
}
