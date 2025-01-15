import os from 'os'

export const CONTROL_KEY = os.platform() === 'darwin' ? 'Meta' : 'Control'
export const DELETE_KEY = os.platform() === 'darwin' ? 'Backspace' : 'Delete'
