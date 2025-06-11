/** @file A modal for uploading multiple files with conflicts. */
import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Dialog, Popover } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs'
import { Text } from '#/components/Text'
import { VisualTooltip } from '#/components/VisualTooltip'
import { listDirectoryQueryOptions, type ListDirectoryQueryOptions } from '#/hooks/backendHooks'
import AssetIcon from '#/pages/dashboard/components/AssetIcon'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import {
  AssetType,
  extractTitleAndSuffix,
  FileId,
  pathByTitleSchema,
  RelativePath,
  titleSchema,
  type AnyAsset,
  type AssetResolution,
} from '#/services/Backend'
import { getRootEntryInPath } from '#/utilities/path'
import { useText } from '$/providers/text'
import { useQuery } from '@tanstack/react-query'
import { getText, resolveDictionary } from 'enso-common/src/text'

/** Props for a {@link ConflictingMultipleUploadModal}. */
export interface ConflictingMultipleUploadFormProps {
  readonly archivePaths: readonly RelativePath[]
  readonly parentDirectoryQueryOptions: ListDirectoryQueryOptions
  readonly onSubmit: (resolutions: readonly AssetResolution[]) => void
  readonly onCancel?: () => void
}

/** A modal for uploading multiple files with conflicts. */
export function ConflictingMultipleUploadForm(props: ConflictingMultipleUploadFormProps) {
  const { archivePaths: archivePathsRaw, parentDirectoryQueryOptions, onSubmit, onCancel } = props
  const archivePaths = [
    ...new Set(archivePathsRaw.map((path) => RelativePath(getRootEntryInPath(path)))),
  ]

  const text = useText()

  const { data: siblings = [] } = useQuery(listDirectoryQueryOptions(parentDirectoryQueryOptions))

  const titleSchemaInstance = titleSchema({ id: FileId(''), siblings })
  const form = Form.useForm({
    schema: (z) =>
      z.object({
        acceptedConflicts: z.array(z.string()),
        newPaths: z.record(
          z.string(),
          z.null().or(pathByTitleSchema({ id: FileId(''), siblings })),
        ),
      }),
    defaultValues: {
      acceptedConflicts: [...archivePaths.keys()].map(String),
      newPaths: Object.fromEntries(archivePaths.map((path) => [path, null])),
    },
    onSubmit: ({ acceptedConflicts, newPaths }) => {
      const selection = new Set(acceptedConflicts.map((conflict) => Number(conflict)))
      onSubmit(
        archivePaths.map((path, i) => {
          const newPath = newPaths[path]
          return selection.has(i) && newPath != null ?
              { type: 'rename', path: path, newPath: RelativePath(newPath) }
            : { type: 'skip', path: path }
        }),
      )
    },
  })
  const newPaths = form.watch('newPaths')

  return (
    <Form form={form}>
      <Checkbox.Group
        aria-label={text.getText('assetList')}
        form={form}
        name="acceptedConflicts"
        className="w-full"
      >
        {archivePaths.map((path, i) => {
          const newPath = RelativePath(newPaths[path] ?? path)
          const { title, suffix } = extractTitleAndSuffix(newPath)
          const isValid = siblings.every((sibling) => sibling.title !== title)
          const pseudoAsset: Pick<AnyAsset, 'title' | 'type'> = {
            type: AssetType.file,
            title: path,
          }
          return (
            <label className="flex w-full cursor-pointer select-none">
              <Checkbox value={String(i)} />
              <div className="flex w-full items-center gap-2">
                <AssetIcon asset={pseudoAsset} />
                <VisualTooltip tooltip={isValid ? null : text.getText('nameShouldBeUnique')}>
                  <Text color={isValid ? 'primary' : 'danger'}>{title}</Text>
                </VisualTooltip>
                <Popover.Trigger>
                  <Button variant="icon" icon="edit" className="ml-auto" />

                  <Popover placement="bottom start">
                    <Form
                      method="dialog"
                      defaultValues={{ newName: title }}
                      schema={(z) => z.object({ newName: titleSchemaInstance })}
                      onSubmit={({ newName }) => {
                        form.setValue('newPaths', { [path]: `${newName}${suffix}` })
                      }}
                    >
                      <Text>{text.getText('newNameDescription')}</Text>
                      <Input label={text.getText('newName')} name="newName" autoFocus="select" />
                      <Form.Submit>{text.getText('apply')}</Form.Submit>

                      <Form.FormError />
                    </Form>
                  </Popover>
                </Popover.Trigger>
              </div>
            </label>
          )
        })}
        <Form.FieldError name="acceptedConflicts" />
      </Checkbox.Group>
      <Button.Group>
        <Form.Submit />
        <Button variant="outline" onPress={onCancel} className="mr-auto">
          {text.getText('cancel')}
        </Button>
      </Button.Group>
      <Form.FormError />
    </Form>
  )
}

/** Options for resolving conflicts. */
export interface ResolveConflictsOptions
  extends Omit<ConflictingMultipleUploadFormProps, 'onCancel' | 'onSubmit'> {}

/** Function for resolving conflicts. */
// eslint-disable-next-line react-refresh/only-export-components
export async function resolveConflicts(options: ResolveConflictsOptions) {
  return new Promise<readonly AssetResolution[]>((resolve, reject) => {
    const cancel = () => {
      reject(new Error('Conflicting multiple upload was cancelled.'))
    }
    setModal(
      <Dialog title={getText(resolveDictionary(), 'conflictingAssetsFound')} onDismiss={cancel}>
        {({ close }) => (
          <ConflictingMultipleUploadForm
            {...options}
            onSubmit={(value) => {
              unsetModal()
              resolve(value)
            }}
            onCancel={() => {
              close()
              cancel()
            }}
          />
        )}
      </Dialog>,
    )
  }).finally(unsetModal)
}
