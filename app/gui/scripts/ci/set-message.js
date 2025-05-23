// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-nocheck
import { dedent } from './dedent.js'

/**
 * Sets or updates a section in a GitHub PR comment
 * @param {Object} options - Function options
 * @param {string} options.header - The header text for the section
 * @param {string} options.body - The body text for the section
 * @param {number|string} options.prNumber - PR number
 * @param {Object} options.repo - Repository info
 * @param {Object} options.github - GitHub API instance
 * @param {string} [options.id] - Section identifier (defaults to header)
 * @param {Object} options.context - Context object, see: https://github.com/actions/toolkit/blob/main/packages/github/src/context.ts
 */
export async function setMessage({
  header,
  body,
  prNumber,
  repo,
  github,
  id = header,
  recreate = false,
  context,
}) {
  // Only run on pull requests
  if (context.eventName !== 'pull_request') {
    console.log('Not a pull request, skipping')
    return
  }

  const commentList = await github.paginate(
    'GET /repos/:owner/:repo/issues/:issue_number/comments',
    // eslint-disable-next-line camelcase
    { ...repo, issue_number: prNumber },
  )

  const messageMarker = `<!-- __message__ ${prNumber} __message__ -->`
  const sectionMarkerStart = `<!-- start ${id} -->`
  const sectionMarkerEnd = `<!-- end ${id} -->`

  const commentBody = dedent`
    ${sectionMarkerStart}

    ${header}

    ${body}

    ${sectionMarkerEnd}
  `

  const comment = commentList.find((comment) => comment.body.includes(messageMarker))

  // Comment doesn't exist, create it
  if (!comment) {
    console.log('Comment does not exist, creating it')
    await github.rest.issues.createComment({
      ...repo,
      // eslint-disable-next-line camelcase
      issue_number: prNumber,
      body: dedent`
        ${messageMarker}

        ${commentBody}
      `,
    })

    return
  }

  // Comment exists, recreate it
  if (recreate) {
    console.log('Comment exists, recreating it')
    // Replace existing comment
    await github.rest.issues.updateComment({
      ...repo,
      // eslint-disable-next-line camelcase
      comment_id: comment.id,
      body: dedent`
        ${messageMarker}

        ${commentBody}
      `,
    })

    return
  }

  console.log(`Comment exists, updating it, section ${id}`)

  // Update existing comment
  const indexOfStart = comment.body.indexOf(sectionMarkerStart)
  const indexOfEnd = comment.body.indexOf(sectionMarkerEnd)

  let updatedBody

  if (indexOfStart !== -1 && indexOfEnd !== -1) {
    // Section exists, replace it
    updatedBody =
      comment.body.slice(0, indexOfStart) +
      commentBody +
      comment.body.slice(indexOfEnd + sectionMarkerEnd.length)
  } else {
    // Section doesn't exist, append it
    updatedBody = comment.body + '\n\n' + commentBody
  }

  await github.rest.issues.updateComment({
    ...repo,
    // eslint-disable-next-line camelcase
    comment_id: comment.id,
    body: updatedBody,
  })
}
