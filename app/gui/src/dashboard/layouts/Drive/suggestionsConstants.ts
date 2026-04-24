/** @file Constants related to suggestions for the asset search bar. */
import type * as assetSearchBar from '#/layouts/AssetSearchBar'

export const SUGGESTIONS_FOR_TYPE: assetSearchBar.Suggestion[] = [
  {
    key: 'type:project',
    render: () => 'type:project',
    addToQuery: (query) => query.add('types', ['project']),
    deleteFromQuery: (query) => query.delete('types', ['project']),
  },
  {
    key: 'type:folder',
    render: () => 'type:folder',
    addToQuery: (query) => query.add('types', ['folder']),
    deleteFromQuery: (query) => query.delete('types', ['folder']),
  },
  {
    key: 'type:file',
    render: () => 'type:file',
    addToQuery: (query) => query.add('types', ['file']),
    deleteFromQuery: (query) => query.delete('types', ['file']),
  },
  {
    key: 'type:secret',
    render: () => 'type:secret',
    addToQuery: (query) => query.add('types', ['secret']),
    deleteFromQuery: (query) => query.delete('types', ['secret']),
  },
  {
    key: 'type:datalink',
    render: () => 'type:datalink',
    addToQuery: (query) => query.add('types', ['datalink']),
    deleteFromQuery: (query) => query.delete('types', ['datalink']),
  },
]
