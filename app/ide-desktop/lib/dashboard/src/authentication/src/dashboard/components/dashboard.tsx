// FIXME [NP3]: Remove these lints.
/* eslint-disable @typescript-eslint/no-non-null-assertion */
/* eslint-disable @typescript-eslint/no-unsafe-member-access */
/* eslint-disable @typescript-eslint/no-unsafe-assignment */
/** @file Main dashboard container responsible for listing user's projects as well as other
 * interactive components. */

import * as react from "react";
import reactDom from "react-dom";
import * as projectManager from "enso-studio-content/src/project_manager";

import * as auth from "../../authentication/providers/auth";
import withRouter from "../../navigation";
import * as backend from "../service";
import Templates from "./templates";
import ProjectActionButton from "./projectActionButton";
import * as loggerProvider from "../../providers/logger";



// ==========================
// === dashboardContainer ===
// ==========================

export interface DashboardProps {
  runningOnDesktop: boolean;
  projectManager: projectManager.ProjectManager | undefined;
}

const columns = [
  "Projects",
  "Last modified",
  "Shared with",
  "Labels",
  "Data access",
  "Usage plan",
  "Engine",
  "IDE",
];

const tableHeaders = columns.map((columnName, index) => {
  return (
    <th
      key={index}
      className="px-6 align-middle border border-solid py-3 text-xs border-l-0 border-r-1 border-t-0 border-b-0 whitespace-nowrap font-semibold text-left"
    >
      {columnName}
    </th>
  );
});

const dashboardContainer = (props: DashboardProps) => {
  const { signOut } = auth.useAuth();
  const { accessToken, organization } = auth.useFullUserSession();
  const logger = loggerProvider.useLogger();
  const backendService = backend.createBackend(accessToken, logger);
  const { runningOnDesktop, projectManager } = props;
  const [projectsList, setProjectsList] = react.useState<backend.Project[]>([]);

  const getNewProjectName = (templateName: string | undefined): string => {
    const projectNameTemplateStart = templateName
      ? `${templateName}_`
      : "New_Project_";
    const projectNameTemplate = new RegExp(
      `^${projectNameTemplateStart}(?<projectIndex>\\d+)$`
    );
    let lastProjectIndex = 1;
    projectsList.forEach((projectItem) => {
      let projectIndex: string | number | undefined;
      projectIndex = projectNameTemplate.exec(projectItem.name)?.groups
        ?.projectIndex;
      if (!projectIndex) {
        return;
      }
      projectIndex = parseInt(projectIndex, 10);
      if (projectIndex > lastProjectIndex) {
        lastProjectIndex = projectIndex;
      }
    });
    lastProjectIndex++;

    return projectNameTemplateStart + lastProjectIndex.toString();
  };

  const handleCreateProject = async (templateName: string | undefined) => {
    const newProjectName = getNewProjectName(templateName);

    if (!runningOnDesktop) {
      const newProject = await backendService.createProject({
        projectName: newProjectName,
        projectTemplateName: templateName?.toLowerCase(),
      });
      setProjectsList([...projectsList, ...[newProject]]);
    } else {
      const createdProject = await projectManager!.createProject(
        newProjectName,
        templateName
      );
      const newProject = {
        organizationId: organization.id,
        projectId: createdProject.result.projectId,
        name: createdProject.result.projectName,
        state: { type: "Created" },
        packageName: "Main",
        address: null,
        ami: null,
        ideVersion: null,
        engineVersion: null,
      } as backend.Project;
      setProjectsList([...projectsList, ...[newProject]]);
    }
  };

  react.useEffect(() => {
    void (async (): Promise<void> => {
      let newProjectsList: backend.Project[] = [];

      if (!runningOnDesktop) {
        newProjectsList = await backendService.listProjects();
      } else {
        const localProjects: any[] = (await projectManager!.listProjects())
          .result.projects;
        for (const item of localProjects) {
          newProjectsList.push({
            organizationId: organization.id,
            projectId: item.id,
            name: item.name,
            state: { type: "Created" },
            packageName: "Main",
            address: null,
            ami: null,
            ideVersion: null,
            engineVersion: null,
          } as backend.Project);
        }
      }
      reactDom.unstable_batchedUpdates(() => {
        setProjectsList(newProjectsList);
      });
    })();
  }, [accessToken]);

  let itemsTable: any = (
    <tr>
      <td colSpan={columns.length}>
        You have no project yet. Go ahead and create one using the form above.
      </td>
    </tr>
  );

  if (projectsList.length > 0) {
    const setProjectOpening = (projectItemIndex: number): void => {
      setProjectsList((currProjectList) => {
        const newProjectList = [...currProjectList];
        newProjectList[projectItemIndex]!.state.type =
          backend.ProjectState.openInProgress;
        return newProjectList;
      });
    };
    const setProjectOpen = (projectItemIndex: number): void => {
      setProjectsList((currProjectList) => {
        const newProjectList = [...currProjectList];
        newProjectList[projectItemIndex]!.state.type =
          backend.ProjectState.opened;
        return newProjectList;
      });
    };
    const setProjectClosed = (projectItemIndex: number): void => {
      setProjectsList((currProjectList) => {
        const newProjectList = [...currProjectList];
        newProjectList[projectItemIndex]!.state.type =
          backend.ProjectState.closed;
        return newProjectList;
      });
    };

    itemsTable = projectsList.map((item, index) => {
      return (
        <tr
          key={item.projectId}
          className="transition duration-300 ease-in-out hover:bg-gray-100"
        >
          <td className="px-6 text-left flex items-center align-middle whitespace-nowrap border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
            <ProjectActionButton
              project={item}
              onOpen={() => {
                setProjectOpen(index);
              }}
              onOpenStart={() => {
                setProjectOpening(index);
              }}
              onClose={() => {
                setProjectClosed(index);
              }}
            />
            {item.name}
          </td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
            2022-08-10, 13:30
          </td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
            aa
          </td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
            aa
          </td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
            aa
          </td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0">
            aa
          </td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0"></td>
          <td className="px-6 border border-solid border-l-0 border-r-1 border-t-0 border-b-0"></td>
        </tr>
      );
    });
  }

  return (
    <>
      <Templates onChange={handleCreateProject} />
      <table className="items-center w-full bg-transparent border-collapse">
        <thead>
          <tr>{tableHeaders}</tr>
        </thead>
        <tbody>{itemsTable}</tbody>
      </table>
      <button onClick={signOut}>Log out</button>
    </>
  );
};

export default withRouter(dashboardContainer);
