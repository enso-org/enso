import os from 'os'

export const CONTROL_KEY = 'ControlOrMeta'
export const DELETE_KEY = os.platform() === 'darwin' ? 'Backspace' : 'Delete'
