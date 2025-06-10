import HttpClient from '#/utilities/HttpClient'
import { createGlobalState } from '@vueuse/core'

function generateSessionID() {
  const sessionID = sessionStorage.getItem('sessionID')
  if (sessionID) {
    return sessionID
  }

  const newSessionID = crypto.randomUUID()
  sessionStorage.setItem('sessionID', newSessionID)
  return newSessionID
}

function createHttpClient() {
  const sessionID = generateSessionID()
  return new HttpClient({
    'x-enso-ide-version': $config.VERSION ?? '',
    'x-enso-session-id': sessionID,
  })
}

export const useHttpClient = createGlobalState(createHttpClient)
