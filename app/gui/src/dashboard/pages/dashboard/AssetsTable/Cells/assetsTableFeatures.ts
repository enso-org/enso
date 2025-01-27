import { functionalUpdate, makeStateUpdater, type OnChangeFn, type RowData, type TableFeature, type Updater } from '@tanstack/react-table'

export interface EditingNameTableState {
  editingRowId: string | null;
}

export interface EditingNameRowState {
  isEditingName: boolean;
}

// define types for our new feature's table options
export interface EditingNameOptions {
  onEditingNameChange?: OnChangeFn<EditingNameRowState>;
}

// Define types for our new feature's table APIs
export interface EditingNameInstance {
  setEditingName: (updater: Updater<EditingNameRowState>) => void;
  toggleEditingName: (value?: EditingNameRowState) => void;
}


export const EditingNameFeature: TableFeature<EditingNameTableState> = {
  getInitialState: (state) => ({
    ...state,
    editingRowId: null,
  }),

  getDefaultOptions: (table) => ({
    onEditingNameChange: makeStateUpdater<EditingNameTableState>('isEditingName', table),
  }),

  createTable: (table) => {
    table.setEditingName = (updater) => {
      const safeUpdater: Updater<EditingNameTableState> = (old) => {
        let newState = functionalUpdate(updater, old);
        return newState;
      };
      return table.options.onEditingNameChange?.(safeUpdater);
    };
    table.toggleEditingName = (value) => {
      table.setEditingName((state) => ({
        ...state,
        editingRowId: value?.editingRowId ?? null,
      }));
    };
  },

  createRow: <TData extends RowData>(row, table): void => {
    row.toggleEditingName = () => {
  },
}
