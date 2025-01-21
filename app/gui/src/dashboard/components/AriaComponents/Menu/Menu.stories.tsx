/**
 * @file
 * Stories for the Menu component.
 */
import Camera from '#/assets/camera.svg'
import Eye from '#/assets/eye.svg'
import EyeClosed from '#/assets/eye_crossed.svg'
import Folder from '#/assets/folder.svg'
import type { Meta, StoryObj } from '@storybook/react'

import { expect, userEvent, within } from '@storybook/test'
import type { MenuProps } from '.'
import { Menu } from '.'
import { Button } from '../Button'

const meta = {
  title: 'Components/Menu',
  component: Menu,
  parameters: {
    layout: 'centered',
  },
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const button = canvas.getByRole('button', { name: 'Open Menu' })
    await userEvent.click(button)
  },
} satisfies Meta<MenuProps<object>>

export default meta

type Story = StoryObj<MenuProps<object>>

const MenuContent = () => (
  <>
    <Menu.Section title="File">
      <Menu.Item>New File</Menu.Item>
      <Menu.Item>Open...</Menu.Item>
      <Menu.Item>Save</Menu.Item>
    </Menu.Section>
    <Menu.Section title="Edit">
      <Menu.Item>Cut</Menu.Item>
      <Menu.Item>Copy</Menu.Item>
      <Menu.Item>Paste</Menu.Item>
    </Menu.Section>
  </>
)

const MenuContentWithIcons = () => (
  <>
    <Menu.Section title="File">
      <Menu.Item icon={<span>📄</span>}>New File</Menu.Item>
      <Menu.Item icon={<span>📂</span>}>Open...</Menu.Item>
      <Menu.Item icon={<span>💾</span>}>Save</Menu.Item>
    </Menu.Section>
    <Menu.Section title="Edit">
      <Menu.Item icon={<span>✂️</span>}>Cut</Menu.Item>
      <Menu.Item icon={<span>📋</span>}>Copy</Menu.Item>
      <Menu.Item icon={<span>📌</span>}>Paste</Menu.Item>
    </Menu.Section>
  </>
)

const MenuContentWithShortcuts = () => (
  <>
    <Menu.Section title="File">
      <Menu.Item shortcut="⌘N">New File</Menu.Item>
      <Menu.Item shortcut="⌘O">Open...</Menu.Item>
      <Menu.Item shortcut="⌘S">Save</Menu.Item>
    </Menu.Section>
    <Menu.Section title="Edit">
      <Menu.Item shortcut="⌘X">Cut</Menu.Item>
      <Menu.Item shortcut="⌘C">Copy</Menu.Item>
      <Menu.Item shortcut="⌘V">Paste</Menu.Item>
    </Menu.Section>
  </>
)

const MenuContentWithIconsAndShortcuts = () => (
  <>
    <Menu.Section title="File">
      <Menu.Item icon={Camera} shortcut="⌘N">
        New File
      </Menu.Item>
      <Menu.Item icon={Folder} shortcut="⌘O">
        Open...
      </Menu.Item>
      <Menu.Item icon={Folder} shortcut="⌘O">
        Very long item that should be truncated
      </Menu.Item>

      <Menu.Item icon={({ isHovered }) => (isHovered ? EyeClosed : Eye)} shortcut="⌘S">
        Save
      </Menu.Item>
    </Menu.Section>
    <Menu.Separator />
    <Menu.Section title="Edit">
      <Menu.Item icon={<span>✂️</span>} shortcut="⌘X">
        Cut
      </Menu.Item>
      <Menu.Item icon={<span>📋</span>} shortcut="⌘C">
        Copy
      </Menu.Item>
      <Menu.Item icon={<span>📌</span>} shortcut="⌘V">
        Paste
      </Menu.Item>
    </Menu.Section>
  </>
)

export const Default: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>
      <Menu>
        <MenuContent />
      </Menu>
    </Menu.Trigger>
  ),
}

export const WithIconsTrigger: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>
      <Menu>
        <MenuContentWithIcons />
      </Menu>
    </Menu.Trigger>
  ),
}

export const WithShortcutsTrigger: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>
      <Menu>
        <MenuContentWithShortcuts />
      </Menu>
    </Menu.Trigger>
  ),
}

export const WithIconsAndShortcutsTrigger: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>
      <Menu>
        <MenuContentWithIconsAndShortcuts />
      </Menu>
    </Menu.Trigger>
  ),
}

export const WithSelection: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>
      <Menu selectionMode="multiple">
        <MenuContentWithIconsAndShortcuts />
      </Menu>
    </Menu.Trigger>
  ),
}

function MenuContentWithDescription() {
  return (
    <>
      <Menu.Item description="This is a description">New File</Menu.Item>
      <Menu.Item>
        Very long item that should be truncated Lorem ipsum dolor sit amet consectetur adipisicing
        elit. Quisquam, quos.
      </Menu.Item>
      <Menu.Item>Should be title only</Menu.Item>
      <Menu.Item description="Very long description that should be truncated Lorem ipsum dolor sit amet consectetur adipisicing elit. Quisquam, quos.">
        New File
      </Menu.Item>

      <Menu.Item>
        <div>Custom content</div>
      </Menu.Item>

      <Menu.Item icon={Eye} description="This is a description" shortcut="⌘N">
        New File
      </Menu.Item>

      <Menu.Separator />

      <Menu.SubmenuTrigger>
        <Menu.Item icon={Folder} description="This is a description" shortcut="⌘O">
          Open Submenu
        </Menu.Item>
        <Menu selectionMode="multiple" placement="right">
          <Menu.Item description="This is a description" icon={Eye}>
            Submenu item
          </Menu.Item>
        </Menu>
      </Menu.SubmenuTrigger>
    </>
  )
}

export const WithDescription: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>
      <Menu selectionMode="multiple">
        <MenuContentWithDescription />
      </Menu>
    </Menu.Trigger>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const button = canvas.getByRole('button', { name: 'Open Menu' })
    await userEvent.click(button)

    const submenu = canvas.getAllByRole('menuitem')
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    await userEvent.click(submenu.at(-1)!)
  },
}

function MenuContentWithDynamicContent() {
  const sections = [
    { id: 1, name: 'New' },
    { id: 2, name: 'Open' },
    { id: 3, name: 'Close' },
    { id: 4, name: 'Save' },
    { id: 5, name: 'Duplicate' },
    { id: 6, name: 'Rename' },
    { id: 7, name: 'Move' },
  ]

  const items = [
    { id: 1, name: 'Apple' },
    { id: 2, name: 'Banana' },
    { id: 3, name: 'Cherry' },
    { id: 4, name: 'Date' },
    { id: 5, name: 'Elderberry' },
    { id: 6, name: 'Fig' },
    { id: 7, name: 'Grape' },
  ]

  return (
    <Menu items={sections} selectionMode="single">
      {(section) => (
        <Menu.Section id={section.id} items={items} title={section.name}>
          {(item) => <Menu.Item id={`${section.id}-${item.id}`}>{item.name}</Menu.Item>}
        </Menu.Section>
      )}
    </Menu>
  )
}

export const DynamicContent: Story = {
  render: () => (
    <Menu.Trigger>
      <Button>Open Menu</Button>

      <MenuContentWithDynamicContent />
    </Menu.Trigger>
  ),
  play: async ({ canvasElement }) => {
    const canvas = within(canvasElement)
    const button = canvas.getByRole('button', { name: 'Open Menu' })

    await userEvent.click(button)

    await expect(canvas.getByRole('menu')).toBeInTheDocument()
  },
}
