import os from 'os'

export const CONTROL_KEY = 'Mod'
export const DELETE_KEY = os.platform() === 'darwin' ? 'Backspace' : 'Delete'
