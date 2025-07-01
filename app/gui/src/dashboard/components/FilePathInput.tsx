/** @file A file path input component with an integrated file browser. */
import { Input } from '#/components/aria'
import FocusRing from '#/components/styled/FocusRing'
import { twMerge } from '#/utilities/tailwindMerge'
import { vueComponent } from '#/utilities/vue'
import { useText } from '$/providers/react'
import { AnimatePresence, motion } from 'framer-motion'
import { type CSSProperties, lazy, useRef, useState } from 'react'

const ANIMATION_DURATION = 0.2

const FileBrowserWidget = lazy(() =>
  import('@/components/widgets/FileBrowserWidget.vue').then(({ default: vue }) =>
    vueComponent(vue),
  ),
)

/** Props for {@link FilePathInput}. */
export interface FilePathInputProps {
  readonly readOnly?: boolean
  readonly value: string
  readonly onChange: (value: string) => void
  readonly validationErrorClassName?: string | undefined
  readonly errors?: React.ReactNode[]
}

/** A file path input component with an integrated file browser. */
export default function FilePathInput(props: FilePathInputProps) {
  const { readOnly = false, value, onChange, validationErrorClassName, errors = [] } = props
  const { getText } = useText()
  const [fileBrowserPath, setFileBrowserPath] = useState(() => value)
  const [isFileBrowserOpened, setFileBrowserOpened] = useState(false)
  const inputRef = useRef<HTMLInputElement>(null)

  // Helper function to generate rounded className based on roundedBottom prop
  const roundedInputClassName = (roundBottom: boolean = false, outline: boolean = true) => {
    const baseClasses =
      'h-6 w-full grow border-0.5 border-primary/20 bg-transparent px-2 outline-offset-2 transition-[border-color,outline] duration-200 read-only:read-only'
    const outlineClasses =
      outline ?
        'focus:border-primary/50 focus:outline focus:outline-2 focus:outline-offset-0 focus:outline-primary'
      : ''
    const roundedClasses = roundBottom ? 'rounded-input' : 'rounded-t-input'
    return `${baseClasses} ${outlineClasses} ${roundedClasses}`
  }

  return (
    <div
      className="flex flex-col"
      style={
        // eslint-disable-next-line no-restricted-syntax
        {
          // eslint-disable-next-line @typescript-eslint/naming-convention
          '--file-browser-min-width': '280px',
        } as CSSProperties
      }
    >
      <FocusRing within={true}>
        <div
          style={{ position: 'relative' }}
          className="rounded-input focus-within:focus-ring-outset"
          onFocus={() => {
            setFileBrowserOpened(true)
          }}
          onBlur={() => {
            setFileBrowserOpened(false)
          }}
        >
          <Input
            ref={inputRef}
            type="text"
            readOnly={readOnly}
            value={fileBrowserPath}
            size={1}
            className={twMerge(
              roundedInputClassName(!isFileBrowserOpened, false),
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
                onMouseDown={(e: MouseEvent) => {
                  // Prevent focus loss when clicking inside the file browser
                  e.preventDefault()
                  inputRef.current?.focus()
                }}
              >
                <FileBrowserWidget
                  type="file"
                  writeMode={true}
                  choosenPath={fileBrowserPath}
                  onPathAccepted={(p: string) => {
                    setFileBrowserPath(p)
                    onChange(p)
                  }}
                  fileTypes={[]}
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
