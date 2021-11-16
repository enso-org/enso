/// This module defines helper methods for templates view.

import { ProjectManager } from './project_manager'

const PM = ProjectManager.default()

const PROJECTS_LIST = 'projects-list'
const PROJECTS_LIST_NEW_PROJECT = 'projects-list-new-project'

const CARD_SPREADSHEETS = 'card-spreadsheets'
const CARD_GEO = 'card-geo'
const CARD_VISUALIZE = 'card-visualize'

const ALL_CARDS = [CARD_SPREADSHEETS, CARD_GEO, CARD_VISUALIZE]

/**
 * The sore for hidden elements.
 *
 * When the templates view is loaded, it hides some top-level elements, as
 * their style is messing up scrolling. Hidden elements will be restored before
 * loading the IDE.
 */
let hiddenElements: HTMLElement[] = []

/** Status box div element for displaying errors. */
let statusBox: HTMLElement = undefined

/**
 * Display the templates view.
 *
 * Main entry point. Loads the templates HTML markup, loads the projects list
 * and sets callbacks on the template cards.
 *
 * @param openProject the callback that opens IDE with the provided project.
 */
async function loadTemplatesView(openProject: (project: string) => void): Promise<void> {
    const templatesView = require('./templates-view.html')
    hideRootHtml()
    document.body.innerHTML += templatesView
    statusBox = document.getElementById('templates-status-box')

    try {
        await loadProjectsList(openProject)
    } catch (error) {
        displayStatusBox('Failed to load projects.')
    }

    setTemplateCardHandlers(openProject)
}

/**
 * Remove the top-level root div from the scene.
 */
function hideRootHtml(): void {
    const rootDiv = document.getElementById('root')
    hiddenElements.push(rootDiv)
    rootDiv.remove()
}

/**
 * Restore the elements removed by the `hideRootHtml` function.
 */
function restoreRootHtml(): void {
    let templatesView = document.getElementById('templates-view')
    hiddenElements
        .slice()
        .reverse()
        .forEach(element => document.body.append(element))
    hiddenElements = []
    templatesView.remove()
}

/**
 * Show the message in the statsus box div element.
 *
 * @param text the message to display
 */
function displayStatusBox(text: string): void {
    statusBox.innerHTML = text
    statusBox.style.visibility = 'visible'
}

/**
 * Clear the status box div element.
 */
function clearStatusBox(): void {
    statusBox.style.visibility = 'hidden'
}

/**
 * Load the projects list.
 *
 * Uses Project Manager to get the list of user projects and displays
 * them in the projects side menu.
 *
 * @param openProject the callback that opens IDE with the provided project
 */
async function loadProjectsList(openProject: (project: string) => void): Promise<void> {
    const projectsListNode = document.getElementById(PROJECTS_LIST)

    const newProjectNode = document.getElementById(PROJECTS_LIST_NEW_PROJECT)
    newProjectNode.setAttribute('style', 'cursor: pointer;')
    newProjectNode.onclick = () => {
        clearStatusBox()
        PM.createProject('Unnamed', 'default')
            .then((response: any) => {
                if (response.error !== undefined) {
                    console.error('Project manager openProject failed', response)
                    displayStatusBox(response.error.message)
                } else {
                    restoreRootHtml()
                    openProject(response.result.projectName)
                }
            })
            .catch((error: any) => {
                console.error('onclick', PROJECTS_LIST_NEW_PROJECT, error)
                displayStatusBox('Failed to create a new project.')
            })
    }

    const projectsListResult = await PM.listProjects()
    const projectsList = projectsListResult.result.projects.map((project: any) =>
        buildProjectListNode(project.name, openProject)
    )

    projectsList.forEach((element: any) => {
        projectsListNode.insertBefore(element, newProjectNode)
    })
}

/**
 * Build `li` HTML element for the projects side menu.
 *
 * @param projectName the name of the project
 * @param openProject the callback that opens IDE with the provided project
 */
function buildProjectListNode(
    projectName: string,
    openProject: (project: string) => void
): HTMLLIElement {
    const li = document.createElement('li')
    li.setAttribute('style', 'cursor: pointer;')
    li.onclick = () => {
        restoreRootHtml()
        openProject(projectName)
    }

    const img = document.createElement('img')
    img.setAttribute('src', '/assets/project.svg')

    const text = document.createTextNode(projectName)

    li.appendChild(img)
    li.appendChild(text)

    return li
}

/**
 * Set `onclick` callbacks for all template cards.
 *
 * @param openProject the callback that opens IDE with the provided project
 */
function setTemplateCardHandlers(openProject: (project: String) => void): void {
    ALL_CARDS.forEach((cardId: string) => {
        const cardElement = document.getElementById(cardId)
        setTemplateCardHandler(cardElement, openProject)
    })
}

/**
 * Set the `onclick` callback for the template card.
 *
 * @param element the HTML element of the template card
 * @param openProject the callback that opens IDE with the provided project
 */
function setTemplateCardHandler(
    element: HTMLElement,
    openProject: (project: string) => void
): void {
    element.setAttribute('style', 'cursor: pointer;')
    element.onclick = () => {
        const projectName = getProjectName(element.id)
        const templateName = getProjectTemplate(element.id)
        clearStatusBox()

        PM.createProject(projectName, templateName)
            .then((response: any) => {
                if (response.error !== undefined) {
                    console.error('Project manager createProject failed', response)
                    displayStatusBox(response.error.message)
                } else {
                    restoreRootHtml()
                    openProject(response.result.projectName)
                }
            })
            .catch((error: any) => {
                console.error('onclick', element.id, error)
                displayStatusBox('Failed to open a template.')
            })
    }
}

/**
 * Get the project name by the template card HTML identifier.
 *
 * @param elementId the template card id
 * @return the project name
 */
function getProjectName(elementId: string): string {
    switch (elementId) {
        case CARD_SPREADSHEETS:
            return 'Orders'
        case CARD_GEO:
            return 'Restaurants'
        case CARD_VISUALIZE:
            return 'Stargazers'
        default:
            return 'Unnamed'
    }
}

/**
 * Get the template name by the template card HTML identifier.
 *
 * @param elementId the template card id
 * @return the template name
 */
function getProjectTemplate(elementId: string): string {
    switch (elementId) {
        case CARD_SPREADSHEETS:
            return 'orders'
        case CARD_GEO:
            return 'restaurants'
        case CARD_VISUALIZE:
            return 'stargazers'
        default:
            return 'default'
    }
}

export { loadTemplatesView }
