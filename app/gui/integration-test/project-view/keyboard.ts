import os from 'os'

export const DELETE_KEY = os.platform() === 'darwin' ? 'Backspace' : 'Delete'
