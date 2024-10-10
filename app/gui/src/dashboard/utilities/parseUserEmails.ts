/** @file Parses a string of emails separated by commas or spaces into an array of emails. */

/** Result of parsing a single email entry. */
export interface ParseUserEmailsResult {
  readonly entries: ParseUserEmailsEntry[]
}

/** A single email entry. */
export interface ParseUserEmailsEntry {
  readonly email: string
}

/**
 * Parses emails separated by commas, semicolons, spaces or newlines, also removing any leading/trailing whitespaces and
 * removing any empty strings and should support emails copied from gmail, outlook, apple mail.
 * Supports emails in the following formats:
 *
 * ***NOTE*** This function does not validate the email addresses, it only parses them.
 * @returns an object with the parsed entries and any errors that occurred during parsing.
 */
export function parseUserEmails(emailsString: string): ParseUserEmailsResult {
  const entries = emailsString
    .split(/[,;\s\n]+/)
    .map((entry) => entry.trim())
    .filter((entry) => entry.length > 0)
    .map((email) => ({ email }))
  return { entries }
}
