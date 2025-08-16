/** @file A file path input component with an integrated file browser. */
import { Input } from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import { twMerge } from '#/utilities/tailwindMerge'
import { vueComponent } from '#/utilities/vue'
import { useText } from '$/providers/react'
import FileBrowserWidgetVue from '@/components/widgets/FileBrowserWidget.vue'
import { AnimatePresence, motion } from 'framer-motion'
import { type CSSProperties, useRef, useState } from 'react'
import { ROUNDED_INPUT_BASE_CLASSES } from './JSONSchemaInput'

const ANIMATION_DURATION = 0.2

// eslint-disable-next-line no-restricted-syntax
const FileBrowserWidget = vueComponent(FileBrowserWidgetVue).default

/** Props for {@link FilePathInput}. */
export interface FilePathInputProps {
  readonly readOnly?: boolean
  readonly value: string
  readonly onChange: (value: string) => void
  readonly validationErrorClassName?: string | undefined
  readonly errors?: readonly React.ReactNode[]
}

/** A file path input component with an integrated file browser. */
export default function FilePathInput(props: FilePathInputProps) {
  const { readOnly = false, value, onChange, validationErrorClassName, errors = [] } = props
  const { getText } = useText()
  const [fileBrowserPath, setFileBrowserPath] = useState(() => value)
  const [isFileBrowserOpened, setFileBrowserOpened] = useState(false)
  const inputRef = useRef<HTMLInputElement>(null)
  const rootRef = useRef<HTMLDivElement>(null)

  const roundedInputClassName = (roundBottom: boolean) =>
    twMerge(ROUNDED_INPUT_BASE_CLASSES, roundBottom ? 'rounded-input' : 'rounded-t-input')

  const fileBrowserZIndex = 1

  /* eslint-disable @typescript-eslint/naming-convention */
  /* eslint-disable-next-line no-restricted-syntax */
  const fileBrowserStyles = {
    '--file-browser-min-width': '280px',
    '--z-index-file-browser': fileBrowserZIndex,
    '--file-browser-background-color': 'var(--color-dashboard-background)',
    '--file-browser-text-color': 'black',
    '--file-browser-corner-radius': 'var(--input-corner-radius)',
    '--file-browser-top-bar-color': 'var(--color-primary)',
  } as CSSProperties
  /* eslint-enable @typescript-eslint/naming-convention */

  return (
    <div
      ref={rootRef}
      className={twMerge('flex flex-col', isFileBrowserOpened && 'mb-4')}
      style={fileBrowserStyles}
      tabIndex={-1}
      onBlur={(event) => {
        // Check if the focus is still inside the current component, otherwise close the file browser.
        if (rootRef.current && !rootRef.current.contains(event.relatedTarget)) {
          setFileBrowserOpened(false)
        }
      }}
    >
      <FocusRing within={true}>
        <div
          style={{ position: 'relative' }}
          className="rounded-input focus-within:focus-ring-outset"
          onFocus={() => {
            setFileBrowserOpened(true)
          }}
        >
          <Input
            ref={inputRef}
            type="text"
            readOnly={readOnly}
            value={fileBrowserPath}
            size={1}
            className={twMerge(
              roundedInputClassName(!isFileBrowserOpened),
              validationErrorClassName,
            )}
            placeholder={getText('enterText')}
            onChange={(event) => {
              const newValue: string = event.currentTarget.value
              setFileBrowserPath(newValue)
              onChange(newValue)
            }}
          />
          <AnimatePresence>
            {isFileBrowserOpened && (
              <motion.div
                initial={{ opacity: 0, height: 0 }}
                animate={{ opacity: 1, height: 'auto' }}
                exit={{ opacity: 0, height: 0 }}
                transition={{ duration: ANIMATION_DURATION }}
              >
                <FileBrowserWidget
                  type="file"
                  writeMode={true}
                  choosenPath={fileBrowserPath}
                  onPathAccepted={(p: string) => {
                    setFileBrowserPath(p)
                    onChange(p)
                  }}
                  allowOverride={true}
                />
              </motion.div>
            )}
          </AnimatePresence>
        </div>
      </FocusRing>
      {...errors}
    </div>
  )
}
