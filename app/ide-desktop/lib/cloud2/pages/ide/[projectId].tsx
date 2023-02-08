// Next.js page file for IDE page.
// See more about Next.js pages: https://nextjs.org/docs/basic-features/pages.

// TODO: replace console.log with some logger.
// Track here: https://github.com/enso-org/cloud-ui/issues/19.
/* eslint-disable no-console */

import Head from "next/head";
import Link from "next/link";
import { useRouter } from "next/router";
import { useEffect, useState } from "react";
// unstable_batchedUpdates are stable besides its unstable_ prefix.
// See https://medium.com/swlh/react-state-batch-update-b1b61bd28cd2
// for instance.
import { unstable_batchedUpdates as batchedUpdates } from "react-dom";

import { getProjectDetails, useAuth } from "../../service";

// =================
// === Constants ===
// =================

const IDE_CDN_URL = "https://ensocdn.s3.us-west-1.amazonaws.com/ide";
const DEFAULT_ERROR = "Unexpected error occured while loading the project.";

// ===============
// === IdePage ===
// ===============

// IdePage component renders Enso IDE if the cloud project is running.
const IdePage: React.FC = () => {
  const { accessToken, organizationId, userEmail } = useAuth();
  const router = useRouter();
  const [isProjectRunning, setIsProjectRunning] = useState(true);
  const [projectName, setProjectName] = useState("");
  const [isLoading, setIsLoading] = useState(true);
  const [loadingError, setLoadingError] = useState("");
  // const [projectsConfig, _] = useLocalStorageState(
  //     {},
  //     `projects_config_${organizationId}`
  // );

  // Enso IDE polutes the "window" object. So, we need to refresh the page
  // once this component unmounts. We pass a function that just returns the
  // cleanup function that refreshes the page. See more:
  // https://reactjs.org/docs/hooks-effect.html#effects-with-cleanup.
  useEffect(() => {
    return function cleanup(): void {
      if (!("enso" in window)) return;
      location.reload();
    };
  }, []);

  useEffect(() => {
    void (async (): Promise<void> => {
      if (!accessToken) {
        return;
      }

      const projectIdParam = router.query.projectId;

      if (typeof projectIdParam === "undefined") {
        console.error("ProjectId query param was not provided.");
        batchedUpdates(() => {
          setIsLoading(false);
          setLoadingError(DEFAULT_ERROR);
        });
        return;
      }

      const projectId = projectIdParam;

      // It can be an array only if using "catch all routes" Next.js
      // feature.
      // See more: https://nextjs.org/docs/routing/dynamic-routes
      // #catch-all-routes.
      if (Array.isArray(projectId)) {
        console.error("ProjectId query param is an Array.");
        batchedUpdates(() => {
          setIsLoading(false);
          setLoadingError(DEFAULT_ERROR);
        });
        return;
      }

      if (!projectId) {
        console.error("ProjectId query param is empty.");
        batchedUpdates(() => {
          setIsLoading(false);
          setLoadingError(DEFAULT_ERROR);
        });
        return;
      }

      const project = await getProjectDetails(accessToken!, projectId);

      if (!project) {
        console.error("Couldn't get the project by the provided id.");
        batchedUpdates(() => {
          setIsLoading(false);
          setLoadingError(DEFAULT_ERROR);
        });
        return;
      }

      const projectIdeVersion = "2022.7.1-nightly.2022.12.30";
      const projectEngineVersion = "2022.7.1-nightly.2022.12.30";

      batchedUpdates(() => {
        setIsLoading(false);
        setProjectName("Main");
      });

      const jsonUri = `${project.address}json`;
      const binaryUri = `${project.address}binary`;
      const stylesheetLink = document.createElement("link");
      stylesheetLink.rel = "stylesheet";
      stylesheetLink.href = `${IDE_CDN_URL}/${projectIdeVersion}/style.css`;
      const indexScript = document.createElement("script");
      indexScript.src = `${IDE_CDN_URL}/${projectIdeVersion}/index.js.gz`;

      const ideScript = document.createElement("script");
      ideScript.textContent = `
                enso.main({
                    wasm_url: "${IDE_CDN_URL}/${projectIdeVersion}/ide.wasm",
                    wasm_glue_url: "${IDE_CDN_URL}/${projectIdeVersion}" +
                        "/wasm_imports.js.gz",
                    language_server_rpc: "${jsonUri}",
                    language_server_data: "${binaryUri}",
                    cloud_mode: true,
                    use_loader: true,
                    authentication_enabled: false,
                    project: "${project.packageName}",
                    preferred_engine_version: "${projectEngineVersion}",
                    data_gathering: true,
                    email: "${userEmail}"
                });
            `;
      document.head.appendChild(stylesheetLink);
      stylesheetLink.onload = (): void => {
        document.body.appendChild(indexScript);
        indexScript.onload = (): void => {
          document.body.appendChild(ideScript);
        };
      };
    })();
  }, [accessToken, organizationId, router.query.projectId, userEmail]);

  let isLoadingJsx = null;
  if (isLoading) {
    isLoadingJsx = (
      <p className="text-lg text-blue-custom-1 my-10 text-center">Loading...</p>
    );
  }

  let errorMessageJsx = null;
  if (loadingError) {
    errorMessageJsx = (
      <p className="text-lg text-red-500 my-10 text-center">{loadingError}</p>
    );
  }
  if (!isProjectRunning) {
    errorMessageJsx = (
      <p className="text-lg text-red-500 my-10 text-center">
        This project is not running. Please{" "}
        <Link href="/">
          <a>go back</a>
        </Link>{" "}
        to the project list and run it.
      </p>
    );
  }

  let pageTitle = "Enso Cloud IDE";
  if (projectName) {
    pageTitle += `: ${projectName}`;
  }

  return (
    <>
      <Head>
        <title>{pageTitle}</title>
      </Head>
      <div id="ide">
        <div id="root" />
      </div>
      {isLoadingJsx}
      {errorMessageJsx}
    </>
  );
};

export default IdePage;
