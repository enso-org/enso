import * as random from 'lib0/random'
import { mockDataWSHandler as originalMockDataWSHandler } from 'shared/dataServer/mock'
import type { LibraryComponentGroup, Uuid, response } from 'shared/languageServerTypes'
import type { SuggestionEntry } from 'shared/languageServerTypes/suggestions'
import type { MockTransportData } from 'src/util/net'
import type { QualifiedName } from 'src/util/qualifiedName'
import { mockFsDirectoryHandle } from '../src/util/convert/fsAccess'
import mockDb from '../stories/mockSuggestions.json' assert { type: 'json' }

const mockProjectId = random.uuidv4() as Uuid
const standardBase = 'Standard.Base' as QualifiedName

export function placeholderGroups(): LibraryComponentGroup[] {
  return [
    { color: '#4D9A29', name: 'Input', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Web', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Parse', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Select', library: standardBase, exports: [] },
    { color: '#B37923', name: 'Join', library: standardBase, exports: [] },
    { color: '#9735B9', name: 'Transform', library: standardBase, exports: [] },
    { color: '#4D9A29', name: 'Output', library: standardBase, exports: [] },
  ]
}

export const mockLSHandler: MockTransportData = async (method, data, transport) => {
  switch (method) {
    case 'session/initProtocolConnection':
      return {
        contentRoots: [{ type: 'Project', id: mockProjectId }],
      } satisfies response.InitProtocolConnection
    case 'executionContext/create':
      setTimeout(
        () => transport.emit('executionContext/executionComplete', { contextId: data.contextId }),
        100,
      )
      return {
        contextId: data.contextId,
      }
    case 'search/getSuggestionsDatabase':
      return {
        entries: mockDb.map((suggestion, id) => ({
          id,
          suggestion: suggestion as SuggestionEntry,
        })),
        currentVersion: 1,
      } satisfies response.GetSuggestionsDatabase
    case 'runtime/getComponentGroups':
      return { componentGroups: placeholderGroups() } satisfies response.GetComponentGroups
    case 'executionContext/push':
    case 'executionContext/pop':
    case 'executionContext/recompute':
    case 'capability/acquire':
      return {}
    default:
      return Promise.reject(`Method not mocked: ${method}`)
  }
}

let mainFile = `\
from Standard.Base import all
from Standard.Base.Runtime.Ref import Ref

from Standard.Test import Bench

options = Bench.options . set_warmup (Bench.phase_conf 1 2) . set_measure (Bench.phase_conf 3 2)

collect_benches = Bench.build builder->
    range_size = 100000000
    data = 0.up_to range_size

    builder.group "Range" options group_builder->
        group_builder.specify "iterate" <|
            cell = Ref.new 0
            data . each _->
                x = cell.get
                cell.put x+1

            cell.get . should_equal range_size

main =
    benches = collect_benches
    result = run_main benches`

export function getMainFile() {
  return mainFile
}

export function setMainFile(newMainFile: string) {
  return (mainFile = newMainFile)
}

const directory = mockFsDirectoryHandle(
  {
    src: {
      get 'Main.enso'() {
        return mainFile
      },
    },
  },
  '(root)',
)

export const mockDataHandler = originalMockDataWSHandler(async (segments) => {
  if (!segments.length) return
  let file
  try {
    let dir = directory
    for (const segment of segments.slice(0, -1)) {
      dir = await dir.getDirectoryHandle(segment)
    }
    const fileHandle = await dir.getFileHandle(segments.at(-1)!)
    file = await fileHandle.getFile()
  } catch {
    return
  }
  return await file?.arrayBuffer()
})
