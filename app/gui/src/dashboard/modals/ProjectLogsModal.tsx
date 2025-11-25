/** @file A modal for showing logs for a project. */
import { Button } from '#/components/Button'
import { Dialog } from '#/components/Dialog'
import { Loader } from '#/components/Loader'
import { useText } from '$/providers/react'
import type { Monaco } from '@monaco-editor/react'
import { Editor } from '@monaco-editor/react'
import { useInfiniteQuery } from '@tanstack/react-query'
import type { Backend, ProjectSessionId } from 'enso-common/src/services/Backend'
import { useRef } from 'react'

const MONACO_OPTIONS: NonNullable<Parameters<typeof Editor>[0]['options']> = {
  wordWrap: 'on',
  readOnly: true,
}

const ENSO_LOG_MONACO_LANGUAGE: Parameters<Monaco['languages']['setMonarchTokensProvider']>[1] = {
  defaultToken: '',
  tokenizer: {
    root: [
      { include: '@level' },
      { include: '@timestamp' },
      { include: '@namespace' },
      { include: '@functionReference' },
    ],
    level: [
      [/\[TRACE\]/, 'comment'],
      [/\[DEBUG\]/, 'comment'],
      [/\[INFO\]/, 'comment'],
      [/\[WARN\]/, 'comment'],
      [/\[ERROR\]/, 'comment'],
    ],
    timestamp: [[/\[\d+-\d+-\d+T\d+:\d+:\d+Z\]/, 'keyword']],
    namespace: [[/\[org.[^\]]+\]/, 'type']],
    functionReference: [[/\(org.[^\]]+\)/, 'variable']],
  },
}

/** Props for a {@link ProjectLogsModal}. */
export interface ProjectLogsModalProps {
  readonly backend: Backend
  readonly projectSessionId: ProjectSessionId
  readonly projectTitle: string
}

/** A modal for showing logs for a project. */
export default function ProjectLogsModal(props: ProjectLogsModalProps) {
  const { getText } = useText()

  return (
    <Dialog title={getText('logs')} type="fullscreen">
      <ProjectLogsModalInternal {...props} />
    </Dialog>
  )
}

/** A modal for showing logs for a project. */
function ProjectLogsModalInternal(props: ProjectLogsModalProps) {
  const { backend, projectSessionId, projectTitle } = props
  const { getText } = useText()
  const editorRef = useRef<Monaco>()

  const logsPages = useInfiniteQuery({
    queryKey: ['projectLogs', { projectSessionId, projectTitle, infinite: true }],
    queryFn: ({ pageParam }) =>
      backend.getProjectSessionLogs(projectSessionId, { scrollId: pageParam }, projectTitle),
    initialPageParam: ((): string | null => null)(),
    getNextPageParam: (page) => (page.hits.length === 0 ? null : page.scrollId),
  })
  const logs = logsPages.data?.pages.flatMap((page) => page.hits).join('\n') ?? ''
  const isLoading = logsPages.isLoading

  return (
    <div className="flex h-full flex-col gap-2">
      <Button.Group className="grow-0 items-center">
        <Button
          variant="icon"
          icon="find"
          aria-label={getText('search')}
          onPress={async () => {
            const editor = editorRef.current?.editor.getEditors()[0]
            await editor?.getAction('actions.find')?.run()
          }}
        />
        <Button
          variant="icon"
          icon="refresh"
          aria-label={getText('reload')}
          onPress={() => logsPages.refetch()}
        />
        <Button
          variant="icon"
          icon="data_download"
          aria-label={getText('loadMore')}
          tooltip={logsPages.hasNextPage ? null : getText('noMoreEntriesToLoad')}
          onPress={() => logsPages.fetchNextPage()}
          isDisabled={!logsPages.hasNextPage}
        />
      </Button.Group>
      {isLoading ?
        <Loader />
      : <Editor
          beforeMount={(monaco) => {
            editorRef.current = monaco
            monaco.editor.defineTheme('transparentBackground', {
              base: 'vs',
              inherit: true,
              rules: [],
              // The name comes from a third-party API and cannot be changed.
              // eslint-disable-next-line @typescript-eslint/naming-convention
              colors: { 'editor.background': '#00000000' },
            })
            monaco.languages.register({ id: 'ensolog' })
            monaco.languages.setMonarchTokensProvider('ensolog', ENSO_LOG_MONACO_LANGUAGE)
          }}
          value={logs}
          language="ensolog"
          theme="transparentBackground"
          options={MONACO_OPTIONS}
        />
      }
    </div>
  )
}
