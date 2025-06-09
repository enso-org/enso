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
import { titleSchema, type AssetConflict, type AssetResolution } from '#/services/Backend'
import { useText } from '$/providers/text'
import { useQuery } from '@tanstack/react-query'

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

  const { getText } = useText()

  const { data: siblings = null } = useQuery(listDirectoryQueryOptions(parentDirectoryQueryOptions))

  return (
    <Form
      schema={(z) =>
        z.object({
          acceptedConflicts: z.array(z.string()),
          names: z.object(
            Object.fromEntries(
              conflicts.map((conflict) => [conflict.path, z.string().or(z.null())]),
            ),
          ),
        })
      }
      defaultValues={{
        acceptedConflicts: [],
        names: Object.fromEntries(conflicts.map((conflict) => [conflict.path, null])),
      }}
      onSubmit={({ acceptedConflicts }) => {
        const selection = new Set(acceptedConflicts.map((conflict) => Number(conflict)))
        onSubmit(
          conflicts.map((conflict, i) =>
            selection.has(i) ?
              { type: 'rename', path: conflict.path, name: '' }
            : { type: 'skip', path: conflict.path },
          ),
        )
      }}
    >
      {({ form }) => (
        <>
          <Text.Heading>{getText('conflictingAssetsFound')}</Text.Heading>
          <Checkbox.Group name="acceptedConflicts">
            {conflicts.map((conflict, i) => (
              <Checkbox value={String(i)}>
                <AssetIcon asset={{ type: conflict.type, title: conflict.path }} />
                <Text>{conflict.path}</Text>
                <Popover.Trigger>
                  <Button variant="primary" className="min-w-16">
                    {getText('rename')}
                  </Button>

                  <Popover placement="bottom start">
                    <Form
                      method="dialog"
                      defaultValues={{
                        newName: form.getValues('names')[conflict.path] ?? '',
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
                        form.setValue('names', { [conflict.path]: newName })
                      }}
                    >
                      <Text>{getText('newNameDescription')}</Text>

                      <Input label={getText('newName')} name="newName" autoFocus="select" />

                      <Form.Submit>{getText('apply')}</Form.Submit>

                      <Form.FormError />
                    </Form>
                  </Popover>
                </Popover.Trigger>
              </Checkbox>
            ))}
          </Checkbox.Group>
          <Button.Group className="fixed bottom-0 left-0 right-0 border-t-0.5 border-primary/20 bg-background/90 px-3 py-4 backdrop-blur-md">
            <Dialog.Close variant="ghost" onPress={onCancel} className="mr-auto">
              {getText('cancel')}
            </Dialog.Close>
          </Button.Group>
          <Form.FormError />
        </>
      )}
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
    setModal(<ConflictingMultipleUploadForm {...options} onSubmit={resolve} onCancel={reject} />)
  }).finally(unsetModal)
}
