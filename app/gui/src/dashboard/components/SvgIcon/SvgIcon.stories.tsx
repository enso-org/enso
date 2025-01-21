import type { Meta, StoryObj } from '@storybook/react'
import { useState } from 'react'
import { twJoin } from '../../utilities/tailwindMerge'
import { Button } from '../AriaComponents/Button'
import { Text } from '../AriaComponents/Text'
import SvgMask from '../SvgMask'
import { SvgIcon, type SvgIconProps } from './SvgIcon'

interface Module {
  default: string
}

const ICONS = import.meta.glob('../../assets/*.svg', { eager: true }) satisfies Record<
  string,
  Module
>

export default {
  title: 'Components/SvgIcon',
  component: SvgIcon,
  render: (args) => <SvgIcon {...args} />,
  argTypes: {
    showOutline: {
      control: {
        type: 'boolean',
      },
    },
  },
  parameters: {
    layout: 'centered',
  },
} as Meta<SvgIconProps>

type Story = StoryObj<SvgIconProps>

export const Default: Story = {
  args: {
    icon: 'icon-name',
  },
}

export const AllIcons: Story = {
  render: () => {
    // eslint-disable-next-line react-hooks/rules-of-hooks
    const [showOutline, setShowOutline] = useState(false)
    return (
      <div className="max-w-7xl">
        <div className="flex items-center justify-between">
          <Text.Heading className="mb-4">All Icons</Text.Heading>

          <Button
            variant="icon"
            size="small"
            onPress={() => {
              setShowOutline(!showOutline)
            }}
          >
            {showOutline ? 'Hide Outline' : 'Show Outline'}
          </Button>
        </div>

        <div className="grid grid-cols-6 gap-8">
          {Object.entries(ICONS).map(([key, value]) => {
            const name = key.split('/').pop()?.replace('.svg', '')
            return (
              <div className="flex flex-col items-center gap-1" key={key}>
                <div
                  className={twJoin(
                    'flex items-center justify-center',
                    showOutline === true && 'outline outline-2 outline-primary',
                  )}
                >
                  <SvgMask src={value.default} className="h-4 w-4" />
                </div>
                <Text>{name}</Text>
              </div>
            )
          })}
        </div>
      </div>
    )
  },
}
