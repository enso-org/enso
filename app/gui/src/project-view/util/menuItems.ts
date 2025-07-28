/** @file API for custom menu items in the Electron application. */

export type MenuItem = 'about' | 'closeTab'
export type MenuItemHandler = () => void
