/** @file A modal for uploading multiple files with conflicts. */
import { Button } from '#/components/Button'
import { Checkbox } from '#/components/Checkbox'
import { Dialog, Popover } from '#/components/Dialog'
import { Form } from '#/components/Form'
import { Input } from '#/components/Inputs'
import { Text } from '#/components/Text'
import { listDirectoryQueryOptions, type ListDirectoryQueryOptions } from '#/hooks/backendHooks'
import AssetIcon from '#/pages/dashboard/components/AssetIcon'
import { setModal, unsetModal } from '#/providers/ModalProvider'
import {
  RelativePath,
  titleSchema,
  type AssetConflict,
  type AssetResolution,
} from '#/services/Backend'
import { useText } from '$/providers/text'
import { useQuery } from '@tanstack/react-query'
import { getText, resolveDictionary } from 'enso-common/src/text'

/** Props for a {@link ConflictingMultipleUploadModal}. */
export interface ConflictingMultipleUploadFormProps {
  readonly conflicts: readonly AssetConflict[]
  readonly parentDirectoryQueryOptions: ListDirectoryQueryOptions
  readonly onSubmit: (resolutions: readonly AssetResolution[]) => void
  readonly onCancel?: () => void
}

/** A modal for uploading multiple files with conflicts. */
export function ConflictingMultipleUploadForm(props: ConflictingMultipleUploadFormProps) {
  const { conflicts, parentDirectoryQueryOptions, onSubmit, onCancel } = props

  const text = useText()

  const { data: siblings = null } = useQuery(listDirectoryQueryOptions(parentDirectoryQueryOptions))

  const form = Form.useForm({
    schema: (z) =>
      z.object({
        acceptedConflicts: z.array(z.string()),
        newPaths: z.object(
          Object.fromEntries(conflicts.map((conflict) => [conflict.path, z.string().or(z.null())])),
        ),
      }),
    defaultValues: {
      acceptedConflicts: [...conflicts.keys()].map(String),
      newPaths: Object.fromEntries(conflicts.map((conflict) => [conflict.path, null])),
    },
    onSubmit: ({ acceptedConflicts, newPaths }) => {
      const selection = new Set(acceptedConflicts.map((conflict) => Number(conflict)))
      onSubmit(
        conflicts.map((conflict, i) => {
          const newPath = newPaths[conflict.path]
          return selection.has(i) && newPath != null ?
              { type: 'rename', path: conflict.path, newPath: RelativePath(newPath) }
            : { type: 'skip', path: conflict.path }
        }),
      )
    },
  })
  const newPaths = form.watch('newPaths')

  return (
    <Form form={form}>
      <Checkbox.Group name="acceptedConflicts" className="w-full">
        {conflicts.map((conflict, i) => (
          <label className="flex w-full cursor-pointer select-none">
            <Checkbox value={String(i)} />
            <div className="flex w-full items-center gap-2">
              <AssetIcon asset={{ type: conflict.type, title: conflict.path }} />
              <Text>{newPaths[conflict.path] ?? conflict.path}</Text>
              <Popover.Trigger>
                <Button variant="icon" icon="edit" className="ml-auto" />

                <Popover placement="bottom start">
                  <Form
                    method="dialog"
                    defaultValues={{
                      newName: form.getValues('newPaths')[conflict.path] ?? '',
                    }}
                    schema={(schema) =>
                      schema.object({
                        newName: titleSchema({
                          asset: conflict.existingAsset,
                          siblings,
                        }),
                      })
                    }
                    onSubmit={({ newName }) => {
                      form.setValue('newPaths', { [conflict.path]: newName })
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
        ))}
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
    setModal(
      <Dialog title={getText(resolveDictionary(), 'conflictingAssetsFound')}>
        {({ close }) => (
          <ConflictingMultipleUploadForm
            {...options}
            onSubmit={resolve}
            onCancel={() => {
              close()
              reject(new Error('Conflicting multiple upload was cancelled.'))
            }}
          />
        )}
      </Dialog>,
    )
  }).finally(unsetModal)
}
