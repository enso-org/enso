import { PRODUCT_NAME } from 'enso-common/src/constants'
import { UUID } from 'enso-common/src/services/Backend'
import { newtypeConstructor, type Newtype } from 'enso-common/src/utilities/data/newtype'
import * as crypto from 'node:crypto'
import * as fs from 'node:fs/promises'
import * as os from 'node:os'
import * as path from 'node:path'
import { afterEach, beforeAll, beforeEach, describe, expect, test } from 'vitest'
import { EnsoRunner, findEnsoExecutable } from '../ensoRunner.js'
import { ProjectService } from '../index.js'

/** A filesystem path. */
type Path = Newtype<string, 'Path'>
const Path = newtypeConstructor<Path>()

// Test timeout for operations involving language server startup
const LANGUAGE_SERVER_TEST_TIMEOUT = 35000

describe('ProjectService', () => {
  let ensoPath: Path | undefined
  let projectService: ProjectService
  let tempDir: string
  let projectsDirectory: Path

  beforeAll(async () => {
    // Try to find the Enso executable providing path to repository root
    const repositoryRoot = path.join(__dirname, '..', '..', '..', '..', '..')
    ensoPath = findEnsoExecutable(repositoryRoot)

    if (ensoPath) {
      // Use real Enso runner if available
      const runner = new EnsoRunner(ensoPath)
      projectService = new ProjectService(runner, [])
    } else {
      // Fail the test if executable is not available
      throw new Error(
        `${PRODUCT_NAME} executable not found. Cannot run tests without Enso runtime.`,
      )
    }
  })

  beforeEach(async () => {
    // Create a temporary directory for test projects
    tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'project-service-test-'))
    projectsDirectory = Path(tempDir)

    if (!ensoPath) {
      // Fail the test if executable is not available
      throw new Error(
        `${PRODUCT_NAME} executable not found. Cannot run tests without Enso runtime.`,
      )
    }
  })

  afterEach(async () => {
    // Clean up temporary directory
    try {
      await fs.rm(tempDir, { recursive: true, force: true, maxRetries: 5, retryDelay: 1000 })
    } catch (error) {
      console.error('Failed to clean up temp directory:', error)
    }
  })

  describe('createProject', () => {
    test('should create a new project with a valid name', async () => {
      const projectName = 'TestProject'
      const result = await projectService.createProject(projectName, projectsDirectory)

      // Verify the result structure
      expect(result).toBeDefined()
      expect(result.projectId).toBeDefined()
      expect(result.projectName).toBe(projectName)
      expect(result.projectNormalizedName).toBe('TestProject')
      expect(result.projectPath).toContain(tempDir)

      // Verify project directory was created
      const projectExists = await fs
        .access(result.projectPath)
        .then(() => true)
        .catch(() => false)
      expect(projectExists).toBe(true)

      // Verify package.yaml was created
      const packageYamlPath = path.join(result.projectPath, 'package.yaml')
      const packageYamlExists = await fs
        .access(packageYamlPath)
        .then(() => true)
        .catch(() => false)
      expect(packageYamlExists).toBe(true)

      // Verify project metadata was created
      const metadataPath = path.join(result.projectPath, '.enso', 'project.json')
      const metadataExists = await fs
        .access(metadataPath)
        .then(() => true)
        .catch(() => false)
      expect(metadataExists).toBe(true)

      // Read and verify metadata content
      const metadataContent = await fs.readFile(metadataPath, 'utf-8')
      const metadata = JSON.parse(metadataContent)
      expect(metadata.id).toBe(result.projectId)
      expect(metadata.kind).toBe('UserProject')
      expect(metadata.created).toBeDefined()
    })

    test('should handle duplicate project names by appending suffix', async () => {
      const projectName = 'DuplicateProject'

      // Create first project
      const result1 = await projectService.createProject(projectName, projectsDirectory)
      expect(result1.projectName).toBe(projectName)

      // Create second project with same name
      const result2 = await projectService.createProject(projectName, projectsDirectory)
      expect(result2.projectName).toBe(`${projectName}_1`)

      // Create third project with same name
      const result3 = await projectService.createProject(projectName, projectsDirectory)
      expect(result3.projectName).toBe(`${projectName}_2`)

      // Verify all projects were created
      const project1Exists = await fs
        .access(result1.projectPath)
        .then(() => true)
        .catch(() => false)
      const project2Exists = await fs
        .access(result2.projectPath)
        .then(() => true)
        .catch(() => false)
      const project3Exists = await fs
        .access(result3.projectPath)
        .then(() => true)
        .catch(() => false)

      expect(project1Exists).toBe(true)
      expect(project2Exists).toBe(true)
      expect(project3Exists).toBe(true)
    })

    test('should normalize project names correctly', async () => {
      const testCases = [
        { input: 'myProject', expectedName: 'myProject', expectedNormalized: 'MyProject' },
        { input: 'Project_123', expectedName: 'Project_123', expectedNormalized: 'Project_123' },
        { input: 'Test Project', expectedName: 'Test Project', expectedNormalized: 'TestProject' },
      ]

      for (const testCase of testCases) {
        const result = await projectService.createProject(testCase.input, projectsDirectory)

        expect(result.projectName).toBe(testCase.expectedName)
        expect(result.projectNormalizedName).toBe(testCase.expectedNormalized)

        // Verify the normalized name is used for the directory
        expect(result.projectPath).toContain(testCase.expectedNormalized)
      }
    })

    test('should reject empty project names', async () => {
      await expect(projectService.createProject('', projectsDirectory)).rejects.toThrow(
        'Project name cannot be empty',
      )

      await expect(projectService.createProject('   ', projectsDirectory)).rejects.toThrow(
        'Project name cannot be empty',
      )
    })

    test('should create project with template if specified', async () => {
      const projectName = 'TemplateProject'
      const template = 'default'

      const result = await projectService.createProject(projectName, projectsDirectory, template)

      expect(result).toBeDefined()
      expect(result.projectName).toBe(projectName)

      // Verify project was created
      const projectExists = await fs
        .access(result.projectPath)
        .then(() => true)
        .catch(() => false)
      expect(projectExists).toBe(true)
    })

    test('should generate unique UUIDs for each project', async () => {
      const result1 = await projectService.createProject('Project1', projectsDirectory)
      const result2 = await projectService.createProject('Project2', projectsDirectory)

      expect(result1.projectId).toBeDefined()
      expect(result2.projectId).toBeDefined()
      expect(result1.projectId).not.toBe(result2.projectId)

      // Verify UUIDs are valid format
      const uuidRegex = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i
      expect(result1.projectId).toMatch(uuidRegex)
      expect(result2.projectId).toMatch(uuidRegex)
    })

    test('should set correct timestamps in project metadata', async () => {
      const beforeCreation = new Date()
      const result = await projectService.createProject('TimestampProject', projectsDirectory)
      const afterCreation = new Date()

      // Read project metadata
      const metadataPath = path.join(result.projectPath, '.enso', 'project.json')
      const metadataContent = await fs.readFile(metadataPath, 'utf-8')
      const metadata = JSON.parse(metadataContent)

      // Parse the created timestamp
      const createdDate = new Date(metadata.created)

      // Verify the timestamp is within the expected range
      expect(createdDate.getTime()).toBeGreaterThanOrEqual(beforeCreation.getTime())
      expect(createdDate.getTime()).toBeLessThanOrEqual(afterCreation.getTime())

      // Verify RFC3339 format
      expect(metadata.created).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{3})?Z$/)
    })
  })

  describe('openProject', () => {
    test(
      'should successfully open an existing project',
      async () => {
        // First create a project
        const createResult = await projectService.createProject('ProjectToOpen', projectsDirectory)

        // Open the project
        const openResult = await projectService.openProject(
          createResult.projectId,
          projectsDirectory,
        )

        // Verify the result structure
        expect(openResult).toBeDefined()
        expect(openResult.languageServerJsonAddress).toBeDefined()
        expect(openResult.languageServerBinaryAddress).toBeDefined()
        expect(openResult.projectName).toBe('ProjectToOpen')
        expect(openResult.projectNormalizedName).toBe('ProjectToOpen')
        expect(openResult.projectNamespace).toBe('local')

        // Verify sockets have valid structure
        expect(openResult.languageServerJsonAddress.host).toBe('127.0.0.1')
        expect(openResult.languageServerJsonAddress.port).toBeGreaterThan(0)
        expect(openResult.languageServerBinaryAddress.host).toBe('127.0.0.1')
        expect(openResult.languageServerBinaryAddress.port).toBeGreaterThan(0)

        // Close the project to clean up
        await projectService.closeProject(createResult.projectId)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )

    test(
      'should update lastOpened timestamp when opening project',
      async () => {
        // Create a project
        const createResult = await projectService.createProject(
          'TimestampTestProject',
          projectsDirectory,
        )

        const beforeOpen = new Date()
        // Open the project
        await projectService.openProject(createResult.projectId, projectsDirectory)
        const afterOpen = new Date()

        // Read project metadata to check lastOpened
        const metadataPath = path.join(createResult.projectPath, '.enso', 'project.json')
        const metadataContent = await fs.readFile(metadataPath, 'utf-8')
        const metadata = JSON.parse(metadataContent)

        expect(metadata.lastOpened).toBeDefined()

        // Parse the lastOpened timestamp
        const lastOpenedDate = new Date(metadata.lastOpened)
        // Verify the timestamp is within the expected range
        expect(lastOpenedDate.getTime()).toBeGreaterThanOrEqual(beforeOpen.getTime())
        expect(lastOpenedDate.getTime()).toBeLessThanOrEqual(afterOpen.getTime())

        // Verify RFC3339 format
        expect(metadata.lastOpened).toMatch(/^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d{3})?Z$/)

        // Clean up
        await projectService.closeProject(createResult.projectId)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )

    test('should fail when opening non-existent project', async () => {
      const nonExistentId = crypto.randomUUID() as UUID

      await expect(projectService.openProject(nonExistentId, projectsDirectory)).rejects.toThrow(
        `Project not found: ${nonExistentId}`,
      )
    })

    test(
      'should be able to open two projects simultaneously',
      async () => {
        // Create two projects
        const project1 = await projectService.createProject(
          'SimultaneousProject1',
          projectsDirectory,
        )
        const project2 = await projectService.createProject(
          'SimultaneousProject2',
          projectsDirectory,
        )

        // Open both projects
        const openResult1 = await projectService.openProject(project1.projectId, projectsDirectory)
        const openResult2 = await projectService.openProject(project2.projectId, projectsDirectory)

        // Verify both projects opened successfully
        expect(openResult1).toBeDefined()
        expect(openResult2).toBeDefined()

        // Verify both have valid language server addresses
        expect(openResult1.languageServerJsonAddress).toBeDefined()
        expect(openResult1.languageServerBinaryAddress).toBeDefined()
        expect(openResult2.languageServerJsonAddress).toBeDefined()
        expect(openResult2.languageServerBinaryAddress).toBeDefined()

        // Verify they have different ports
        expect(openResult1.languageServerJsonAddress.port).not.toBe(
          openResult2.languageServerJsonAddress.port,
        )
        expect(openResult1.languageServerBinaryAddress.port).not.toBe(
          openResult2.languageServerBinaryAddress.port,
        )

        // Verify both projects have correct names
        expect(openResult1.projectName).toBe('SimultaneousProject1')
        expect(openResult2.projectName).toBe('SimultaneousProject2')

        // Verify both projects have 'local' namespace
        expect(openResult1.projectNamespace).toBe('local')
        expect(openResult2.projectNamespace).toBe('local')

        // Verify both language servers are running by checking ports
        const isPortInUse = async (port: number): Promise<boolean> => {
          try {
            const response = await fetch(`http://127.0.0.1:${port}/_health`)
            return response.ok
          } catch {
            return false
          }
        }

        const project1Running = await isPortInUse(openResult1.languageServerJsonAddress.port)
        const project2Running = await isPortInUse(openResult2.languageServerJsonAddress.port)

        expect(project1Running).toBe(true)
        expect(project2Running).toBe(true)

        // Clean up - close both projects
        await projectService.closeProject(project1.projectId)
        await projectService.closeProject(project2.projectId)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT * 2,
    )
  })

  describe('closeProject', () => {
    test(
      'should successfully close an open project',
      async () => {
        // Create and open a project
        const createResult = await projectService.createProject('ProjectToClose', projectsDirectory)

        await projectService.openProject(createResult.projectId, projectsDirectory)

        // Close the project - should not throw
        await expect(projectService.closeProject(createResult.projectId)).resolves.not.toThrow()
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )

    test(
      'should handle closing already closed project gracefully',
      async () => {
        // Create and open a project
        const createResult = await projectService.createProject(
          'DoubleCloseProject',
          projectsDirectory,
        )

        await projectService.openProject(createResult.projectId, projectsDirectory)

        // Close the project once
        await projectService.closeProject(createResult.projectId)

        // Attempt to close again - should handle gracefully
        await expect(projectService.closeProject(createResult.projectId)).resolves.not.toThrow()
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )

    test('should handle closing a project that was never opened', async () => {
      // Create a project but don't open it
      const createResult = await projectService.createProject(
        'NeverOpenedProject',
        projectsDirectory,
      )

      // Attempt to close - should handle gracefully
      await expect(projectService.closeProject(createResult.projectId)).resolves.not.toThrow()
    })

    test(
      'should properly terminate language server process',
      async () => {
        // Create and open a project
        const createResult = await projectService.createProject(
          'ProcessTerminationProject',
          projectsDirectory,
        )

        const openResult = await projectService.openProject(
          createResult.projectId,
          projectsDirectory,
        )

        // Verify server is running by checking if port is in use
        const jsonPort = openResult.languageServerJsonAddress.port

        // Helper function to check if port is in use
        const isPortInUse = async (port: number): Promise<boolean> => {
          try {
            const response = await fetch(`http://127.0.0.1:${port}/_health`)
            return response.ok
          } catch {
            return false
          }
        }

        // Verify server is initially running
        const isRunningBefore = await isPortInUse(jsonPort)
        expect(isRunningBefore).toBe(true)

        // Close the project
        await projectService.closeProject(createResult.projectId)

        // Wait a bit for the process to fully terminate
        await new Promise((resolve) => setTimeout(resolve, 1000))

        // Verify server is no longer running
        const isRunningAfter = await isPortInUse(jsonPort)
        expect(isRunningAfter).toBe(false)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )
  })

  describe('deleteProject', () => {
    test('should successfully delete a project', async () => {
      // Create a project
      const createResult = await projectService.createProject('ProjectToDelete', projectsDirectory)
      const projectPath = createResult.projectPath

      // Verify project exists
      const existsBefore = await fs
        .access(projectPath)
        .then(() => true)
        .catch(() => false)
      expect(existsBefore).toBe(true)

      // Delete the project
      await projectService.deleteProject(createResult.projectId, projectsDirectory)

      // Verify project no longer exists at original location
      const existsAfter = await fs
        .access(projectPath)
        .then(() => true)
        .catch(() => false)
      expect(existsAfter).toBe(false)
    })

    test('should fail when deleting non-existent project', async () => {
      const nonExistentId = crypto.randomUUID() as UUID

      await expect(projectService.deleteProject(nonExistentId, projectsDirectory)).rejects.toThrow(
        `Project '${nonExistentId}' not found`,
      )
    })

    test('should handle multiple projects and only delete the specified one', async () => {
      // Create multiple projects
      const project1 = await projectService.createProject('Project1', projectsDirectory)
      const project2 = await projectService.createProject('Project2', projectsDirectory)
      const project3 = await projectService.createProject('Project3', projectsDirectory)

      // Delete only project2
      await projectService.deleteProject(project2.projectId, projectsDirectory)

      // Verify project1 and project3 still exist
      const project1Exists = await fs
        .access(project1.projectPath)
        .then(() => true)
        .catch(() => false)
      const project2Exists = await fs
        .access(project2.projectPath)
        .then(() => true)
        .catch(() => false)
      const project3Exists = await fs
        .access(project3.projectPath)
        .then(() => true)
        .catch(() => false)

      expect(project1Exists).toBe(true)
      expect(project2Exists).toBe(false)
      expect(project3Exists).toBe(true)
    })

    test('should successfully delete a project with special characters in name', async () => {
      // Create a project with special characters
      const projectName = 'Test Project #1'
      const createResult = await projectService.createProject(projectName, projectsDirectory)
      const projectPath = createResult.projectPath

      // Verify project exists
      const existsBefore = await fs
        .access(projectPath)
        .then(() => true)
        .catch(() => false)
      expect(existsBefore).toBe(true)

      // Delete the project
      await projectService.deleteProject(createResult.projectId, projectsDirectory)

      // Verify project no longer exists
      const existsAfter = await fs
        .access(projectPath)
        .then(() => true)
        .catch(() => false)
      expect(existsAfter).toBe(false)
    })
  })

  describe('duplicateProject', () => {
    test('should successfully duplicate a project', async () => {
      // Create an original project
      const originalName = 'OriginalProject'
      const originalResult = await projectService.createProject(originalName, projectsDirectory)

      // Add some content to the original project to verify it gets copied
      const testFilePath = path.join(originalResult.projectPath, 'src', 'Main.enso')
      await fs.mkdir(path.dirname(testFilePath), { recursive: true })
      await fs.writeFile(testFilePath, 'main = "Hello from original"')

      // Duplicate the project
      const duplicateResult = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )

      // Verify the duplicate result structure
      expect(duplicateResult).toBeDefined()
      expect(duplicateResult.projectId).toBeDefined()
      expect(duplicateResult.projectName).toBe(`${originalName} (copy)`)
      expect(duplicateResult.projectNormalizedName).toBe('OriginalProjectcopy')
      expect(duplicateResult.projectPath).toBeDefined()

      // Verify the duplicate has a different ID
      expect(duplicateResult.projectId).not.toBe(originalResult.projectId)

      // Verify the duplicate project exists
      const duplicateExists = await fs
        .access(duplicateResult.projectPath)
        .then(() => true)
        .catch(() => false)
      expect(duplicateExists).toBe(true)

      // Verify the content was copied
      const duplicatedFilePath = path.join(duplicateResult.projectPath, 'src', 'Main.enso')
      const duplicatedContent = await fs.readFile(duplicatedFilePath, 'utf-8')
      expect(duplicatedContent).toBe('main = "Hello from original"')

      // Verify duplicate metadata
      const metadataPath = path.join(duplicateResult.projectPath, '.enso', 'project.json')
      const metadataContent = await fs.readFile(metadataPath, 'utf-8')
      const metadata = JSON.parse(metadataContent)
      expect(metadata.id).toBe(duplicateResult.projectId)
      expect(metadata.created).toBeDefined()
      expect(metadata.lastOpened).oneOf([undefined, null])
    })

    test('should handle duplicate names when duplicating multiple times', async () => {
      // Create an original project
      const originalName = 'ProjectToDuplicateMultiple'
      const originalResult = await projectService.createProject(originalName, projectsDirectory)

      // First duplicate
      const duplicate1 = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )
      expect(duplicate1.projectName).toBe(`${originalName} (copy)`)

      // Second duplicate of the same original
      const duplicate2 = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )
      expect(duplicate2.projectName).toBe(`${originalName} (copy)_1`)

      // Third duplicate
      const duplicate3 = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )
      expect(duplicate3.projectName).toBe(`${originalName} (copy)_2`)

      // Verify all projects exist
      const allExist = await Promise.all([
        fs
          .access(originalResult.projectPath)
          .then(() => true)
          .catch(() => false),
        fs
          .access(duplicate1.projectPath)
          .then(() => true)
          .catch(() => false),
        fs
          .access(duplicate2.projectPath)
          .then(() => true)
          .catch(() => false),
        fs
          .access(duplicate3.projectPath)
          .then(() => true)
          .catch(() => false),
      ])
      expect(allExist).toEqual([true, true, true, true])
    })

    test('should duplicate a project with special characters in name', async () => {
      // Create a project with special characters
      const originalName = 'Test Project #1 & More'
      const originalResult = await projectService.createProject(originalName, projectsDirectory)

      // Duplicate the project
      const duplicateResult = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )

      expect(duplicateResult.projectName).toBe(`${originalName} (copy)`)

      // Verify the duplicate exists
      const duplicateExists = await fs
        .access(duplicateResult.projectPath)
        .then(() => true)
        .catch(() => false)
      expect(duplicateExists).toBe(true)
    })

    test('should fail when duplicating non-existent project', async () => {
      const nonExistentId = crypto.randomUUID() as UUID

      await expect(
        projectService.duplicateProject(nonExistentId, projectsDirectory),
      ).rejects.toThrow(`Project not found: ${nonExistentId}`)
    })

    test('should preserve project structure when duplicating', async () => {
      // Create a project with a specific structure
      const originalName = 'ProjectWithStructure'
      const originalResult = await projectService.createProject(originalName, projectsDirectory)

      // Add various files and directories to the original
      const srcDir = path.join(originalResult.projectPath, 'src')
      const testDir = path.join(originalResult.projectPath, 'test')
      const configFile = path.join(originalResult.projectPath, 'config.yaml')

      await fs.mkdir(srcDir, { recursive: true })
      await fs.mkdir(testDir, { recursive: true })
      await fs.writeFile(path.join(srcDir, 'Main.enso'), 'main = "Hello"')
      await fs.writeFile(path.join(srcDir, 'Utils.enso'), 'util = "Utility"')
      await fs.writeFile(path.join(testDir, 'Test.enso'), 'test = "Test"')
      await fs.writeFile(configFile, 'setting: value')

      // Duplicate the project
      const duplicateResult = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )

      // Verify all files and directories were copied
      const duplicateSrcMain = path.join(duplicateResult.projectPath, 'src', 'Main.enso')
      const duplicateSrcUtils = path.join(duplicateResult.projectPath, 'src', 'Utils.enso')
      const duplicateTest = path.join(duplicateResult.projectPath, 'test', 'Test.enso')
      const duplicateConfig = path.join(duplicateResult.projectPath, 'config.yaml')

      const [mainContent, utilsContent, testContent, configContent] = await Promise.all([
        fs.readFile(duplicateSrcMain, 'utf-8'),
        fs.readFile(duplicateSrcUtils, 'utf-8'),
        fs.readFile(duplicateTest, 'utf-8'),
        fs.readFile(duplicateConfig, 'utf-8'),
      ])

      expect(mainContent).toBe('main = "Hello"')
      expect(utilsContent).toBe('util = "Utility"')
      expect(testContent).toBe('test = "Test"')
      expect(configContent).toBe('setting: value')
    })

    test('should generate new UUID and creation timestamp for duplicate', async () => {
      // Create an original project
      const originalResult = await projectService.createProject(
        'ProjectForMetadataTest',
        projectsDirectory,
      )

      // Read original metadata
      const originalMetadataPath = path.join(originalResult.projectPath, '.enso', 'project.json')
      const originalMetadataContent = await fs.readFile(originalMetadataPath, 'utf-8')
      const originalMetadata = JSON.parse(originalMetadataContent)

      // Duplicate the project
      await new Promise((resolve) => setTimeout(resolve, 10))
      const beforeDuplicate = new Date()
      const duplicateResult = await projectService.duplicateProject(
        originalResult.projectId,
        projectsDirectory,
      )
      const afterDuplicate = new Date()

      // Read duplicate metadata
      const duplicateMetadataPath = path.join(duplicateResult.projectPath, '.enso', 'project.json')
      const duplicateMetadataContent = await fs.readFile(duplicateMetadataPath, 'utf-8')
      const duplicateMetadata = JSON.parse(duplicateMetadataContent)

      // Verify different UUID
      expect(duplicateMetadata.id).not.toBe(originalMetadata.id)
      expect(duplicateMetadata.id).toBe(duplicateResult.projectId)

      // Verify new creation timestamp
      expect(duplicateMetadata.created).not.toBe(originalMetadata.created)
      const duplicateCreatedDate = new Date(duplicateMetadata.created)
      expect(duplicateCreatedDate.getTime()).toBeGreaterThanOrEqual(beforeDuplicate.getTime())
      expect(duplicateCreatedDate.getTime()).toBeLessThanOrEqual(afterDuplicate.getTime())

      // Verify namespace is preserved
      expect(duplicateMetadata.namespace).toBe(originalMetadata.namespace)
    })
  })

  describe('renameProject', () => {
    test('should successfully rename a project when language server is not running', async () => {
      // Create a project
      const originalName = 'OriginalProjectName'
      const createResult = await projectService.createProject(originalName, projectsDirectory)

      // Add some content to verify it's preserved after rename
      const testFilePath = path.join(createResult.projectPath, 'src', 'Main.enso')
      await fs.mkdir(path.dirname(testFilePath), { recursive: true })
      await fs.writeFile(testFilePath, 'main = "Hello from project"')

      const newName = 'RenamedProjectName'

      // Rename the project
      await projectService.renameProject(createResult.projectId, newName, projectsDirectory)

      // Verify the directory was renamed
      const oldDirectoryPath = createResult.projectPath
      const newDirectoryPath = path.join(path.dirname(oldDirectoryPath), 'RenamedProjectName')

      // Verify metadata is preserved
      const metadataPath = path.join(newDirectoryPath, '.enso', 'project.json')
      const metadataContent = await fs.readFile(metadataPath, 'utf-8')
      const metadata = JSON.parse(metadataContent)
      expect(metadata.id).toBe(createResult.projectId)

      // Verify package name is updated
      const packagePath = path.join(newDirectoryPath, 'package.yaml')
      const packageContent = await fs.readFile(packagePath, 'utf-8')
      expect(packageContent).contain(newName)

      const oldDirExists = await fs
        .access(oldDirectoryPath)
        .then(() => true)
        .catch(() => false)
      const newDirExists = await fs
        .access(newDirectoryPath)
        .then(() => true)
        .catch(() => false)

      expect(oldDirExists).toBe(false)
      expect(newDirExists).toBe(true)

      // Verify content was preserved
      const renamedTestFilePath = path.join(newDirectoryPath, 'src', 'Main.enso')
      const content = await fs.readFile(renamedTestFilePath, 'utf-8')
      expect(content).toBe('main = "Hello from project"')
    })

    test(
      'should defer directory rename when language server is running',
      async () => {
        // Create a project
        const originalName = 'RunningProjectToRename'
        const createResult = await projectService.createProject(originalName, projectsDirectory)

        // Open the project to start the language server
        await projectService.openProject(createResult.projectId, projectsDirectory)

        // Rename the project while it's running
        const newName = 'RenamedRunningProject'
        try {
          await projectService.renameProject(createResult.projectId, newName, projectsDirectory)
        } catch {
          /* Expected error for uninitialized test project */
        }

        const oldDirectoryPath = createResult.projectPath
        // Verify metadata is preserved
        const metadataPath = path.join(oldDirectoryPath, '.enso', 'project.json')
        const metadataContent = await fs.readFile(metadataPath, 'utf-8')
        const metadata = JSON.parse(metadataContent)
        expect(metadata.id).toBe(createResult.projectId)

        // Verify package name is updated
        const packagePath = path.join(oldDirectoryPath, 'package.yaml')
        const packageContent = await fs.readFile(packagePath, 'utf-8')
        expect(packageContent).contain(newName)

        // Verify directory has NOT been renamed yet (deferred)
        const oldDirExists = await fs
          .access(createResult.projectPath)
          .then(() => true)
          .catch(() => false)
        expect(oldDirExists).toBe(true)

        // Close the project to trigger the deferred rename
        await projectService.closeProject(createResult.projectId)

        // Now verify the directory was renamed after closing
        const newDirectoryPath = path.join(path.dirname(createResult.projectPath), newName)
        const oldDirExistsAfter = await fs
          .access(createResult.projectPath)
          .then(() => true)
          .catch(() => false)
        const newDirExistsAfter = await fs
          .access(newDirectoryPath)
          .then(() => true)
          .catch(() => false)

        expect(oldDirExistsAfter).toBe(false)
        expect(newDirExistsAfter).toBe(true)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )

    test('should fail when renaming to an existing project name', async () => {
      // Create two projects
      const _project1 = await projectService.createProject('Project1', projectsDirectory)
      const project2 = await projectService.createProject('Project2', projectsDirectory)

      // Try to rename project2 to project1's name
      await expect(
        projectService.renameProject(project2.projectId, 'Project1', projectsDirectory),
      ).rejects.toThrow("Project with name 'Project1' already exists.")
    })

    test('should fail when renaming non-existent project', async () => {
      const nonExistentId = crypto.randomUUID() as UUID

      await expect(
        projectService.renameProject(nonExistentId, 'NewName', projectsDirectory),
      ).rejects.toThrow(`Project not found: ${nonExistentId}`)
    })

    test('should reject empty new name', async () => {
      // Create a project
      const createResult = await projectService.createProject('ProjectToRename', projectsDirectory)

      // Try to rename with empty name
      await expect(
        projectService.renameProject(createResult.projectId, '', projectsDirectory),
      ).rejects.toThrow('Project name cannot be empty')

      await expect(
        projectService.renameProject(createResult.projectId, '   ', projectsDirectory),
      ).rejects.toThrow('Project name cannot be empty')
    })

    test('should handle special characters in new name', async () => {
      // Create a project
      const originalName = 'SimpleProject'
      const createResult = await projectService.createProject(originalName, projectsDirectory)

      const newName = 'Project #1 & Special'

      // Rename the project
      await projectService.renameProject(createResult.projectId, newName, projectsDirectory)

      const newDirectoryPath = path.join(path.dirname(createResult.projectPath), 'Project1Special')
      // Verify metadata is preserved
      const metadataPath = path.join(newDirectoryPath, '.enso', 'project.json')
      const metadataContent = await fs.readFile(metadataPath, 'utf-8')
      const metadata = JSON.parse(metadataContent)
      expect(metadata.id).toBe(createResult.projectId)

      // Verify package name is updated
      const packagePath = path.join(newDirectoryPath, 'package.yaml')
      const packageContent = await fs.readFile(packagePath, 'utf-8')
      expect(packageContent).contain(newName)

      // Verify the directory was renamed with normalized name
      const newDirExists = await fs
        .access(newDirectoryPath)
        .then(() => true)
        .catch(() => false)
      expect(newDirExists).toBe(true)
    })

    test('should preserve project content after rename', async () => {
      // Create a project with content
      const originalName = 'ProjectWithContent'
      const createResult = await projectService.createProject(originalName, projectsDirectory)

      // Add various files and directories
      const srcDir = path.join(createResult.projectPath, 'src')
      const testDir = path.join(createResult.projectPath, 'test')
      await fs.mkdir(srcDir, { recursive: true })
      await fs.mkdir(testDir, { recursive: true })
      await fs.writeFile(path.join(srcDir, 'Main.enso'), 'main = "Main content"')
      await fs.writeFile(path.join(srcDir, 'Utils.enso'), 'utils = "Utils content"')
      await fs.writeFile(path.join(testDir, 'Test.enso'), 'test = "Test content"')

      const newName = 'RenamedProjectWithContent'

      // Rename the project
      await projectService.renameProject(createResult.projectId, newName, projectsDirectory)

      // Verify all content is preserved
      const newDirectoryPath = path.join(
        path.dirname(createResult.projectPath),
        'RenamedProjectWithContent',
      )
      const mainContent = await fs.readFile(
        path.join(newDirectoryPath, 'src', 'Main.enso'),
        'utf-8',
      )
      const utilsContent = await fs.readFile(
        path.join(newDirectoryPath, 'src', 'Utils.enso'),
        'utf-8',
      )
      const testContent = await fs.readFile(
        path.join(newDirectoryPath, 'test', 'Test.enso'),
        'utf-8',
      )

      expect(mainContent).toBe('main = "Main content"')
      expect(utilsContent).toBe('utils = "Utils content"')
      expect(testContent).toBe('test = "Test content"')
    })

    test(
      'should allow renaming an opened project multiple times',
      async () => {
        // Create a project
        const originalName = 'ProjectToRenameMultipleTimes'
        const createResult = await projectService.createProject(originalName, projectsDirectory)

        // Open the project to start the language server
        await projectService.openProject(createResult.projectId, projectsDirectory)

        // First rename while it's running
        const firstName = 'FirstRename'
        try {
          await projectService.renameProject(createResult.projectId, firstName, projectsDirectory)
        } catch {
          /* Expected error for uninitialized test project */
        }

        // Verify first rename was applied to package.yaml
        const packagePath1 = path.join(createResult.projectPath, 'package.yaml')
        const packageContent1 = await fs.readFile(packagePath1, 'utf-8')
        expect(packageContent1).contain(firstName)

        // Second rename while still running
        const secondName = 'SecondRename'
        try {
          await projectService.renameProject(createResult.projectId, secondName, projectsDirectory)
        } catch {
          /* Expected error for uninitialized test project */
        }

        // Verify second rename was applied
        const packageContent2 = await fs.readFile(packagePath1, 'utf-8')
        expect(packageContent2).contain(secondName)
        expect(packageContent2).not.contain(firstName)

        // Third rename while still running
        const thirdName = 'ThirdRename'
        try {
          await projectService.renameProject(createResult.projectId, thirdName, projectsDirectory)
        } catch {
          /* Expected error for uninitialized test project */
        }

        // Verify third rename was applied
        const packageContent3 = await fs.readFile(packagePath1, 'utf-8')
        expect(packageContent3).contain(thirdName)
        expect(packageContent3).not.contain(secondName)

        // Close the project to trigger the final deferred rename
        await projectService.closeProject(createResult.projectId)

        // Verify the directory was renamed to the final name
        const finalDirectoryPath = path.join(path.dirname(createResult.projectPath), thirdName)
        const finalDirExists = await fs
          .access(finalDirectoryPath)
          .then(() => true)
          .catch(() => false)
        expect(finalDirExists).toBe(true)

        // Verify the original directory no longer exists
        const originalDirExists = await fs
          .access(createResult.projectPath)
          .then(() => true)
          .catch(() => false)
        expect(originalDirExists).toBe(false)

        // Verify metadata is preserved with correct ID
        const metadataPath = path.join(finalDirectoryPath, '.enso', 'project.json')
        const metadataContent = await fs.readFile(metadataPath, 'utf-8')
        const metadata = JSON.parse(metadataContent)
        expect(metadata.id).toBe(createResult.projectId)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT,
    )

    test(
      'should handle renaming multiple running projects independently',
      async () => {
        // Create two projects
        const project1 = await projectService.createProject('RunningProject1', projectsDirectory)
        const project2 = await projectService.createProject('RunningProject2', projectsDirectory)

        // Open both projects
        await projectService.openProject(project1.projectId, projectsDirectory)
        await projectService.openProject(project2.projectId, projectsDirectory)

        // Rename both projects while they're running
        try {
          await projectService.renameProject(
            project1.projectId,
            'RenamedRunning1',
            projectsDirectory,
          )
        } catch {
          /* Expected error for uninitialized test project */
        }
        try {
          await projectService.renameProject(
            project2.projectId,
            'RenamedRunning2',
            projectsDirectory,
          )
        } catch {
          /* Expected error for uninitialized test project */
        }

        // Verify package name is updated
        const package1Path = path.join(project1.projectPath, 'package.yaml')
        const package1Content = await fs.readFile(package1Path, 'utf-8')
        expect(package1Content).contain('RenamedRunning1')

        const package2Path = path.join(project2.projectPath, 'package.yaml')
        const package2Content = await fs.readFile(package2Path, 'utf-8')
        expect(package2Content).contain('RenamedRunning2')

        // Close both projects
        await projectService.closeProject(project1.projectId)
        await projectService.closeProject(project2.projectId)

        // Verify both directories were renamed
        const newPath1 = path.join(path.dirname(project1.projectPath), 'RenamedRunning1')
        const newPath2 = path.join(path.dirname(project2.projectPath), 'RenamedRunning2')

        const exists1 = await fs
          .access(newPath1)
          .then(() => true)
          .catch(() => false)
        const exists2 = await fs
          .access(newPath2)
          .then(() => true)
          .catch(() => false)

        expect(exists1).toBe(true)
        expect(exists2).toBe(true)
      },
      LANGUAGE_SERVER_TEST_TIMEOUT * 2,
    )
  })
})
