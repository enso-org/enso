/**
 * @file
 *
 * Re-exports the Dialog component.
 */
export * from './Close'
export * from './Dialog'
export * from './DialogTrigger'
export * from './Popover'
export * from './variants'
// eslint-disable-next-line no-restricted-syntax
export { useDialogContext, type DialogContextValue } from './DialogProvider'
export {
  // eslint-disable-next-line no-restricted-syntax
  DialogStackProvider,
  // eslint-disable-next-line no-restricted-syntax
  type DialogStackContextType,
  // eslint-disable-next-line no-restricted-syntax
  type DialogStackItem,
} from './DialogStackProvider'
