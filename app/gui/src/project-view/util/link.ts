/**
 * Heuristic that matches strings suitable to be automatically interpreted as links. Recognizes absolute URLs with
 * `http` and `https` protocols.
 */
export const LINKABLE_URL_REGEX =
  /https?:\/\/[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z0-9()]{1,6}\b[-a-zA-Z0-9()@:%_+.~#?&/=]*/g

/** Heuristic that matches strings suitable to be automatically interpreted as email addresses. */
export const LINKABLE_EMAIL_REGEX =
  /(?:[^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*|(".+"))@(?:\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}]|(?:[a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,})/g
