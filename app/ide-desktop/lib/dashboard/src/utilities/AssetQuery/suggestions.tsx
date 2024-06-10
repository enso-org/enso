/** @file A function to get a list of suggestions. */
import type * as reactQuery from '@tanstack/react-query'

import * as backendHooks from '#/hooks/backendHooks'

import Label from '#/components/dashboard/Label'

import * as backendModule from '#/services/Backend'
import type Backend from '#/services/Backend'

import type * as assetQuery from '#/utilities/AssetQuery'
import AssetQuery from '#/utilities/AssetQuery'
import * as fileInfo from '#/utilities/fileInfo'
import * as permissions from '#/utilities/permissions'

// =================
// === Constants ===
// =================

const SUGGESTIONS_FOR_NO: Suggestion[] = [
  {
    render: () => 'no:label',
    addToQuery: query => query.addToLastTerm({ nos: ['label'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ nos: ['label'] }),
  },
  {
    render: () => 'no:description',
    addToQuery: query => query.addToLastTerm({ nos: ['description'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ nos: ['description'] }),
  },
]
const SUGGESTIONS_FOR_HAS: Suggestion[] = [
  {
    render: () => 'has:label',
    addToQuery: query => query.addToLastTerm({ negativeNos: ['label'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeNos: ['label'] }),
  },
  {
    render: () => 'has:description',
    addToQuery: query => query.addToLastTerm({ negativeNos: ['description'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeNos: ['description'] }),
  },
]
const SUGGESTIONS_FOR_TYPE: Suggestion[] = [
  {
    render: () => 'type:project',
    addToQuery: query => query.addToLastTerm({ types: ['project'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['project'] }),
  },
  {
    render: () => 'type:folder',
    addToQuery: query => query.addToLastTerm({ types: ['folder'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['folder'] }),
  },
  {
    render: () => 'type:file',
    addToQuery: query => query.addToLastTerm({ types: ['file'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['file'] }),
  },
  {
    render: () => 'type:secret',
    addToQuery: query => query.addToLastTerm({ types: ['secret'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['secret'] }),
  },
  {
    render: () => 'type:datalink',
    addToQuery: query => query.addToLastTerm({ types: ['datalink'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ types: ['datalink'] }),
  },
]
const SUGGESTIONS_FOR_NEGATIVE_TYPE: Suggestion[] = [
  {
    render: () => 'type:project',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['project'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['project'] }),
  },
  {
    render: () => 'type:folder',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['folder'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['folder'] }),
  },
  {
    render: () => 'type:file',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['file'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['file'] }),
  },
  {
    render: () => 'type:datalink',
    addToQuery: query => query.addToLastTerm({ negativeTypes: ['datalink'] }),
    deleteFromQuery: query => query.deleteFromLastTerm({ negativeTypes: ['datalink'] }),
  },
]

// ==================
// === Suggestion ===
// ==================

/** A suggested query. */
export interface Suggestion {
  readonly render: () => React.ReactNode
  readonly addToQuery: (query: AssetQuery) => AssetQuery
  readonly deleteFromQuery: (query: AssetQuery) => AssetQuery
}

// =========================
// === assetToSuggestion ===
// =========================

/** Convert a {@link backendModule.AnyAsset} to a {@link Suggestion}. */
function assetToSuggestion(
  node: backendModule.AnyAsset,
  key: assetQuery.AssetQueryKey = 'names'
): Suggestion {
  return {
    render: () => `${key === 'names' ? '' : '-:'}${node.title}`,
    addToQuery: oldQuery => oldQuery.addToLastTerm({ [key]: [node.title] }),
    deleteFromQuery: oldQuery => oldQuery.deleteFromLastTerm({ [key]: [node.title] }),
  }
}

// ======================
// === getSuggestions ===
// ======================

// FIXME: use in AssetSearchBar
/** Get all suggestions matching the specified query. */
export function getSuggestions(
  queryClient: reactQuery.QueryClient,
  user: backendModule.User | null,
  backend: Backend,
  query: AssetQuery,
  labels: readonly backendModule.Label[]
) {
  const isCloud = backend.type === backendModule.BackendType.remote
  const allAssetsMatchingQuery = () =>
    Object.values(backendHooks.getBackendAllKnownDirectories(queryClient, user, backend))
      .flat()
      .filter(asset => query.isMatch(asset))
  const suggestAllTitles = (negative = false) =>
    allAssetsMatchingQuery().map(asset =>
      assetToSuggestion(asset, negative ? 'negativeNames' : 'names')
    )
  const terms = AssetQuery.terms(query.query)
  const term = terms.find(otherTerm => otherTerm.values.length === 0) ?? terms[terms.length - 1]
  const termValues = term?.values ?? []
  const shouldOmitNames = terms.some(otherTerm => otherTerm.tag === 'name')
  if (termValues.length !== 0) {
    return shouldOmitNames ? [] : suggestAllTitles()
  } else {
    const negative = term?.tag?.startsWith('-') ?? false
    switch (term?.tag ?? null) {
      case null:
      case '':
      case '-':
      case 'name':
      case '-name': {
        return suggestAllTitles(negative)
      }
      case 'no':
      case '-has': {
        return isCloud ? SUGGESTIONS_FOR_NO : []
      }
      case 'has':
      case '-no': {
        return isCloud ? SUGGESTIONS_FOR_HAS : []
      }
      case 'type': {
        return SUGGESTIONS_FOR_TYPE
      }
      case '-type': {
        return SUGGESTIONS_FOR_NEGATIVE_TYPE
      }
      case 'ext':
      case '-ext':
      case 'extension':
      case '-extension': {
        const extensions = allAssetsMatchingQuery()
          .filter(asset => asset.type === backendModule.AssetType.file)
          .map(asset => fileInfo.fileExtension(asset.title))
        return Array.from(
          new Set(extensions),
          (extension): Suggestion => ({
            render: () =>
              AssetQuery.termToString({
                tag: `${negative ? '-' : ''}extension`,
                values: [extension],
              }),
            addToQuery: oldQuery =>
              oldQuery.addToLastTerm(
                negative ? { negativeExtensions: [extension] } : { extensions: [extension] }
              ),
            deleteFromQuery: oldQuery =>
              oldQuery.deleteFromLastTerm(
                negative ? { negativeExtensions: [extension] } : { extensions: [extension] }
              ),
          })
        )
      }
      case 'modified':
      case '-modified': {
        const modifieds = allAssetsMatchingQuery().map(asset => {
          const date = new Date(asset.modifiedAt)
          return `${date.getFullYear()}-${date.getMonth() + 1}-${date.getDate()}`
        })
        return Array.from(
          new Set(['today', ...modifieds]),
          (modified): Suggestion => ({
            render: () =>
              AssetQuery.termToString({
                tag: `${negative ? '-' : ''}modified`,
                values: [modified],
              }),
            addToQuery: oldQuery =>
              oldQuery.addToLastTerm(
                negative ? { negativeModifieds: [modified] } : { modifieds: [modified] }
              ),
            deleteFromQuery: oldQuery =>
              oldQuery.deleteFromLastTerm(
                negative ? { negativeModifieds: [modified] } : { modifieds: [modified] }
              ),
          })
        )
      }
      case 'owner':
      case '-owner': {
        const owners = allAssetsMatchingQuery().flatMap(node =>
          (node.permissions ?? [])
            .filter(permission => permission.permission === permissions.PermissionAction.own)
            .map(backendModule.getAssetPermissionName)
        )
        return Array.from(
          new Set(owners),
          (owner): Suggestion => ({
            render: () =>
              AssetQuery.termToString({
                tag: `${negative ? '-' : ''}owner`,
                values: [owner],
              }),
            addToQuery: oldQuery =>
              oldQuery.addToLastTerm(negative ? { negativeOwners: [owner] } : { owners: [owner] }),
            deleteFromQuery: oldQuery =>
              oldQuery.deleteFromLastTerm(
                negative ? { negativeOwners: [owner] } : { owners: [owner] }
              ),
          })
        )
      }
      case 'label':
      case '-label': {
        return labels.map(
          (label): Suggestion => ({
            render: () => (
              <Label active color={label.color} onPress={() => {}}>
                {label.value}
              </Label>
            ),
            addToQuery: oldQuery =>
              oldQuery.addToLastTerm(
                negative ? { negativeLabels: [label.value] } : { labels: [label.value] }
              ),
            deleteFromQuery: oldQuery =>
              oldQuery.deleteFromLastTerm(
                negative ? { negativeLabels: [label.value] } : { labels: [label.value] }
              ),
          })
        )
      }
      default: {
        return shouldOmitNames ? [] : suggestAllTitles()
      }
    }
  }
}
