/** @file Constants related to suggestions for the asset search bar. */
import type * as assetSearchBar from '#/layouts/AssetSearchBar'

export const SUGGESTIONS_FOR_NO: assetSearchBar.Suggestion[] = [
  {
    key: 'no:label',
    render: () => 'no:label',
    addToQuery: (query) => query.addToLastTerm({ nos: ['label'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ nos: ['label'] }),
  },
  {
    key: 'no:description',
    render: () => 'no:description',
    addToQuery: (query) => query.addToLastTerm({ nos: ['description'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ nos: ['description'] }),
  },
]
export const SUGGESTIONS_FOR_HAS: assetSearchBar.Suggestion[] = [
  {
    key: 'has:label',
    render: () => 'has:label',
    addToQuery: (query) => query.addToLastTerm({ negativeNos: ['label'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeNos: ['label'] }),
  },
  {
    key: 'has:description',
    render: () => 'has:description',
    addToQuery: (query) => query.addToLastTerm({ negativeNos: ['description'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeNos: ['description'] }),
  },
]
export const SUGGESTIONS_FOR_TYPE: assetSearchBar.Suggestion[] = [
  {
    key: 'type:project',
    render: () => 'type:project',
    addToQuery: (query) => query.addToLastTerm({ types: ['project'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['project'] }),
  },
  {
    key: 'type:folder',
    render: () => 'type:folder',
    addToQuery: (query) => query.addToLastTerm({ types: ['folder'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['folder'] }),
  },
  {
    key: 'type:file',
    render: () => 'type:file',
    addToQuery: (query) => query.addToLastTerm({ types: ['file'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['file'] }),
  },
  {
    key: 'type:secret',
    render: () => 'type:secret',
    addToQuery: (query) => query.addToLastTerm({ types: ['secret'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['secret'] }),
  },
  {
    key: 'type:datalink',
    render: () => 'type:datalink',
    addToQuery: (query) => query.addToLastTerm({ types: ['datalink'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ types: ['datalink'] }),
  },
]
export const SUGGESTIONS_FOR_NEGATIVE_TYPE: assetSearchBar.Suggestion[] = [
  {
    key: 'type:project',
    render: () => 'type:project',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['project'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['project'] }),
  },
  {
    key: 'type:folder',
    render: () => 'type:folder',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['folder'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['folder'] }),
  },
  {
    key: 'type:file',
    render: () => 'type:file',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['file'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['file'] }),
  },
  {
    key: 'type:datalink',
    render: () => 'type:datalink',
    addToQuery: (query) => query.addToLastTerm({ negativeTypes: ['datalink'] }),
    deleteFromQuery: (query) => query.deleteFromLastTerm({ negativeTypes: ['datalink'] }),
  },
]
