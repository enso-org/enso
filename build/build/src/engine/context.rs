use crate::prelude::*;

use crate::engine;
use crate::engine::download_project_templates;
use crate::engine::env;
use crate::engine::sbt::SbtCommandProvider;
use crate::engine::Benchmarks;
use crate::engine::BuildConfigurationResolved;
use crate::engine::BuiltArtifacts;
use crate::engine::Operation;
use crate::engine::ReleaseCommand;
use crate::engine::ReleaseOperation;
use crate::engine::PARALLEL_ENSO_TESTS;
use crate::enso::BenchmarkOptions;
use crate::enso::BuiltEnso;
use crate::enso::IrCaches;
use crate::paths::cache_directory;
use crate::paths::Paths;
use crate::paths::TargetTriple;
use crate::paths::ENSO_DATA_DIRECTORY;
use crate::paths::ENSO_JAVA;
use crate::paths::ENSO_TEST_JUNIT_DIR;
use crate::project::ProcessWrapper;

use ide_ci::actions::workflow::is_in_env;
use ide_ci::actions::workflow::MessageLevel;
use ide_ci::cache;
use ide_ci::github::release::IsReleaseExt;
use ide_ci::platform::DEFAULT_SHELL;
use ide_ci::programs::sbt;
use ide_ci::programs::Sbt;
use std::env::consts::DLL_EXTENSION;
use std::env::consts::EXE_EXTENSION;



pub type FutureEnginePackage = BoxFuture<'static, Result<crate::paths::generated::EnginePackage>>;

pub type EnginePackageProvider = dyn FnMut() -> FutureEnginePackage + Send + Sync + 'static;

/// Pretty print option variant name, i.e. whether it is Some or None.
///
/// Does not print the actual value under Some, so this can be used for `T`s that do not implement
/// `Debug`.
pub fn format_option_variant<T>(value: &Option<T>, f: &mut Formatter) -> std::fmt::Result {
    match value {
        Some(_) => write!(f, "Some(...)"),
        None => write!(f, "None"),
    }
}

#[derive(derive_more::Deref, derive_more::DerefMut)]
#[derive_where(Debug)]
pub struct RunContext {
    #[deref]
    #[deref_mut]
    pub inner:            crate::project::Context,
    pub config:           BuildConfigurationResolved,
    pub paths:            Paths,
    /// If set, the engine package (used for creating bundles) will be obtained through this
    /// provider rather than built from source along the other Engine components.
    #[derive_where(skip)]
    pub external_runtime: Option<Arc<EnginePackageProvider>>,
}

impl RunContext {
    pub fn new(
        inner: crate::project::Context,
        config: impl Into<BuildConfigurationResolved>,
        triple: TargetTriple,
        external_runtime: Option<Arc<EnginePackageProvider>>,
    ) -> Result<Self> {
        let paths = Paths::new_versions(&inner.repo_root, triple.versions)?;
        let context = RunContext { config: config.into(), inner, paths, external_runtime };
        Ok(context)
    }

    pub fn expected_artifacts(&self) -> BuiltArtifacts {
        BuiltArtifacts {
            engine_package:          self.config.build_engine_package.then(|| {
                self.repo_root.built_distribution.enso_engine_triple.engine_package.clone()
            }),
            launcher_package:        self.config.build_launcher_package.then(|| {
                self.repo_root.built_distribution.enso_launcher_triple.launcher_package.clone()
            }),
            project_manager_package: self.config.build_project_manager_package.then(|| {
                self.repo_root
                    .built_distribution
                    .enso_project_manager_triple
                    .project_manager_package
                    .clone()
            }),
            launcher_bundle:         self.config.build_launcher_bundle.then(|| {
                self.repo_root.built_distribution.enso_bundle_triple.launcher_bundle.clone()
            }),
            project_manager_bundle:  self.config.build_project_manager_bundle.then(|| {
                self.repo_root
                    .built_distribution
                    .project_manager_bundle_triple
                    .project_manager_bundle
                    .clone()
            }),
        }
    }

    /// Check that required programs are present (if not, installs them, if supported). Set
    /// environment variables for the build to follow.
    #[instrument(skip(self))]
    pub async fn prepare_build_env(&self) -> Result {
        // Building native images with Graal on Windows requires Microsoft Visual C++ Build Tools
        // available in the environment. If it is not visible, we need to add it.
        if TARGET_OS == OS::Windows && ide_ci::programs::vs::Cl.lookup().is_err() {
            ide_ci::programs::vs::apply_dev_environment().await?;
        }

        // Setup SBT
        cache::goodie::sbt::Sbt.install_if_missing(&self.cache).await?;
        Sbt.require_present().await?;

        // Other programs.
        ide_ci::programs::Git.require_present().await?;
        ide_ci::programs::Cargo.require_present().await?;
        ide_ci::programs::Node.require_present().await?;
        ide_ci::programs::Npm.require_present().await?;

        let prepare_simple_library_server = {
            if self.config.test_jvm {
                let simple_server_path = &self.paths.repo_root.tools.simple_library_server;
                ide_ci::programs::git::new(simple_server_path)
                    .await?
                    .cmd()?
                    .clean()
                    .run_ok()
                    .await?;
                ide_ci::programs::Npm
                    .cmd()?
                    .current_dir(simple_server_path)
                    .install()
                    .run_ok()
                    .left_future()
            } else {
                ready(Result::Ok(())).right_future()
            }
        };
        let prepare_simple_library_server = tokio::spawn(prepare_simple_library_server);

        // Setup flatc (FlatBuffers compiler), required for building the engine.
        let flatc_goodie = cache::goodie::flatc::Flatc {
            version:  engine::deduce_flatbuffers(&self.repo_root.build_sbt).await?,
            platform: TARGET_OS,
        };
        flatc_goodie.install_if_missing(&self.cache).await?;


        self.paths.emit_env_to_actions().await?;
        debug!("Build configuration: {:#?}", self.config);

        // Setup Tests on Windows
        if TARGET_OS == OS::Windows {
            let default_time_factor: usize = 2;
            env::CI_TEST_TIMEFACTOR.set(&default_time_factor)?;
            env::CI_TEST_FLAKY_ENABLE.set(&true)?;
        }

        // TODO [mwu]
        //  Currently we rely on Musl to be present on the host machine. Eventually, we should
        //  consider obtaining it by ourselves.
        // if TARGET_OS == OS::Linux {
        //     let musl = ide_ci::goodies::musl::Musl;
        //     goodies.require(&musl).await?;
        // }


        // Setup GraalVM
        let graalvm =
            engine::deduce_graal(self.octocrab.clone(), &self.repo_root.build_sbt).await?;
        graalvm.install_if_missing(&self.cache).await?;
        let graal_version = engine::deduce_graal_bundle(&self.repo_root.build_sbt).await?;
        let graalpy_version = graal_version.packages;

        // Install GraalPy standalone distribution
        // GraalPy has a version that corresponds to the `graalMavenPackagesVersion` variable in
        // build.sbt
        let graalpy = cache::goodie::graalpy::GraalPy {
            client:  self.octocrab.clone(),
            version: graalpy_version,
            os:      self.paths.triple.os,
            arch:    self.paths.triple.arch,
        };
        graalpy.install_if_missing(&self.cache).await?;
        ide_ci::programs::graalpy::GraalPy.require_present().await?;

        if self.config.test_java_generated_from_rust {
            // Ensure all runtime dependencies are resolved and exported so that they can be
            // appended to classpath
            let sbt = engine::sbt::Context {
                repo_root:         self.paths.repo_root.path.clone(),
                system_properties: default(),
            };
            sbt.call_arg("syntax-rust-definition/Runtime/managedClasspath").await?;
        }

        prepare_simple_library_server.await??;

        Ok(())
    }

    /// During the native-image build, the engine generates arg files. This function uploads them as
    /// artifacts on the CI, so we can inspect them later.
    /// Note that if something goes wrong, the native image arg files may not be present.
    async fn upload_native_image_arg_files(&self) -> Result {
        debug!("Uploading Native Image Arg Files");
        let engine_runner_ni_argfile =
            &self.repo_root.engine.runner.target.native_image_args_txt.path;
        let launcher_ni_argfile = &self.repo_root.engine.launcher.target.native_image_args_txt.path;
        let project_manager_ni_argfile =
            &self.repo_root.lib.scala.project_manager.target.native_image_args_txt.path;
        let native_image_arg_files = [
            (engine_runner_ni_argfile, "Engine Runner native-image-args"),
            (launcher_ni_argfile, "Launcher native-image-args"),
            (project_manager_ni_argfile, "Project Manager native-image-args"),
        ];
        for (argfile, artifact_name) in native_image_arg_files {
            if argfile.exists() {
                ide_ci::actions::artifacts::upload_single_file(argfile, artifact_name).await?;
            } else {
                warn!(
                    "Native Image Arg File for {} not found at {}",
                    artifact_name,
                    argfile.display()
                );
            }
        }
        Ok(())
    }

    pub async fn build(&self) -> Result<BuiltArtifacts> {
        self.prepare_build_env().await?;
        if ide_ci::ci::run_in_ci() {
            // On CI we remove IR caches. They might contain invalid or outdated data, as are using
            // engine version as part of the key. As such, any change made to engine that does not
            // change its version might break the caches.
            // See (private): https://discord.com/channels/401396655599124480/407883082310352928/939618590158630922
            ide_ci::fs::remove_dir_if_exists(cache_directory())?;

            // Remove the benchmark reports. They are not meant currently to be incrementally
            // updated.

            // We remove all "bench-report.xml" files across the repo, as they confuse the
            // benchmark reporter. See the request: https://github.com/enso-org/enso/pull/8707#issuecomment-1882512361
            let bench_report_xml = "**/bench-report.xml";
            ide_ci::fs::remove_glob(bench_report_xml)?;
        }


        let _test_results_upload_guard =
            if self.config.test_jvm || self.config.test_standard_library.is_some() {
                // If we run tests, make sure that old and new results won't end up mixed together.
                let test_results_dir = ENSO_TEST_JUNIT_DIR
                    .get()
                    .unwrap_or_else(|_| self.paths.repo_root.target.test_results.path.clone());
                ide_ci::fs::reset_dir(&test_results_dir)?;

                // If we are run in CI conditions and we prepared some test results, we want to
                // upload them as a separate artifact to ease debugging. And we do want to do that
                // even if the tests fail and we are leaving the scope with an error.
                is_in_env().then(|| {
                    scopeguard::guard(test_results_dir, |test_results_dir| {
                        ide_ci::global::spawn(
                            "Upload test results",
                            upload_test_results(test_results_dir),
                        );
                    })
                })
            } else {
                None
            };

        // Workaround for incremental compilation issue, as suggested by kustosz.
        // We target files like
        // engine/runtime/target/scala-2.13/classes/META-INF/org/enso/interpreter/node/expression/
        // builtin/BuiltinMethods.metadata but need to remove more so sbt can figure out it needs to
        // rebuild.
        // Otherwise, errors like this may occur:
        // sbt:warning: java.lang.ClassNotFoundException:
        // org.enso.interpreter.node.expression.builtin.bool.True
        ide_ci::fs::remove_if_exists(&self.paths.repo_root.engine.runtime.target)?;
        // cleanup distribution from previous build
        // it is fast to assemble it again
        ide_ci::fs::remove_if_exists(&self.paths.repo_root.built_distribution)?;

        // We want to start this earlier, and await only before Engine build starts.
        let perhaps_generate_java_from_rust_job =
            ide_ci::future::perhaps(self.config.generate_java_from_rust, || {
                crate::rust::parser::generate_java(&self.paths.repo_root)
            });

        // Download Project Template Files
        let client = reqwest::Client::new();
        download_project_templates(client.clone(), self.paths.repo_root.path.clone()).await?;

        // let sbt = WithCwd::new(Sbt, &self.paths.repo_root);

        let mut system = sysinfo::System::new();
        system.refresh_memory();
        trace!("Total memory: {}", system.total_memory());
        trace!("Available memory: {}", system.available_memory());
        trace!("Used memory: {}", system.used_memory());
        trace!("Free memory: {}", system.free_memory());

        // Build packages.
        debug!("Bootstrapping Enso project.");
        let sbt = engine::sbt::Context {
            repo_root:         self.paths.repo_root.path.clone(),
            system_properties: vec![sbt::SystemProperty::new(
                "bench.compileOnly",
                self.config.execute_benchmarks_once.to_string(),
            )],
        };

        perhaps_generate_java_from_rust_job.await.transpose()?;
        let perhaps_test_java_generated_from_rust_job =
            ide_ci::future::perhaps(self.config.test_java_generated_from_rust, || {
                crate::rust::parser::run_self_tests(&self.paths.repo_root)
            });

        // If we have much memory, we can try building everything in a single batch. Reducing number
        // of SBT invocations significantly helps build time. However, it is more memory heavy, so
        // we don't want to call this in environments like GH-hosted runners.

        // === Build project-manager distribution and native image ===
        let mut tasks = vec![];
        if self.config.build_engine_package() {
            tasks.push("buildEngineDistribution");
        }
        if self.config.build_native_runner {
            tasks.push("engine-runner/buildNativeImage");
        }
        if self.config.build_project_manager_package() {
            tasks.push("buildProjectManagerDistribution");
        }
        if self.config.build_launcher_package() {
            tasks.push("buildLauncherDistribution");
        }

        if !tasks.is_empty() {
            debug!("Building distributions and native images.");
            if crate::ci::big_memory_machine() {
                sbt.call_arg(Sbt::concurrent_tasks(tasks)).await?;
            } else {
                sbt.call_arg(Sbt::sequential_tasks(tasks)).await?;
            }
        }

        // === End of Build project-manager distribution and native image ===

        let ret = self.expected_artifacts();

        // Native images built by GraalVM on Windows use MSVC build tools. Thus, the generated
        // binaries have MSVC CRT (C++ standard library) linked. This means that we need to ship
        // the MSVC CRT DLLs along with the binaries.
        if TARGET_OS == OS::Windows {
            debug!("Copying MSVC CRT DLLs to the distribution.");
            for package in ret.packages() {
                let package_dir = package.dir();
                let binary_extensions = [EXE_EXTENSION, DLL_EXTENSION];
                let binaries: Vec<PathBuf> = binary_extensions
                    .into_iter()
                    .flat_map(|extension| {
                        let pattern = package_dir.join_iter(["**", "*"]).with_extension(extension);
                        glob::glob(pattern.as_str()).expect("Incorrect glob pattern")
                    })
                    .map(|p| p.map(|p| p.to_owned()))
                    .try_collect()?;

                debug!(?binaries, "Found executables in the package.");
                for binary in binaries {
                    ide_ci::packaging::add_msvc_redist_dependencies(&binary).await?;
                }
            }
        }

        let enso = BuiltEnso { paths: self.paths.clone() };

        // === Unit tests and Enso tests ===
        debug!("Running unit tests and Enso tests.");
        // We store Scala test result but not immediately fail on it, as we want to run all the
        // tests (including standard library ones) even if Scala tests fail.
        let scala_test_result = if self.config.test_jvm {
            // Make sure that `sbt buildEngineDistributionNoIndex` is run before
            // `project-manager/test`. Note that we do not have to run
            // `buildEngineDistribution` (with indexing), because it is unnecessary.
            sbt.call_arg("buildEngineDistributionNoIndex").await?;

            // Run unit tests
            sbt.call_arg("set Global / parallelExecution := false; test").await.inspect_err(|e| {
                ide_ci::actions::workflow::error(format!("Scala Tests failed: {e:?}"))
            })
        } else {
            // No tests - no fail.
            Ok(())
        };

        match &self.config.test_standard_library {
            Some(selection) => {
                enso.run_tests(IrCaches::No, &sbt, PARALLEL_ENSO_TESTS, selection.clone()).await?;
            }
            None => {}
        }

        perhaps_test_java_generated_from_rust_job.await.transpose()?;

        // === Run benchmarks ===
        let build_benchmark_task = if self.config.build_benchmarks {
            let build_benchmark_task_names = [
                "runtime-benchmarks/compile",
                "language-server/Benchmark/compile",
                "searcher/Benchmark/compile",
                "std-benchmarks/compile",
            ];
            if crate::ci::big_memory_machine() {
                Some(Sbt::concurrent_tasks(build_benchmark_task_names))
            } else {
                Some(Sbt::sequential_tasks(build_benchmark_task_names))
            }
        } else {
            None
        };
        let execute_benchmark_tasks =
            self.config.execute_benchmarks.iter().flat_map(|b| b.sbt_task());
        let build_and_execute_benchmark_task =
            build_benchmark_task.as_deref().into_iter().chain(execute_benchmark_tasks);
        let benchmark_command = Sbt::sequential_tasks(build_and_execute_benchmark_task);
        if !benchmark_command.is_empty() {
            debug!("Running benchmarks.");
            sbt.call_arg(benchmark_command).await?;
        } else {
            debug!("No SBT tasks to run.");
        }

        if self.config.execute_benchmarks.contains(&Benchmarks::Enso) {
            enso.run_benchmarks(BenchmarkOptions { dry_run: false }).await?;
        } else if self.config.check_enso_benchmarks {
            enso.run_benchmarks(BenchmarkOptions { dry_run: true }).await?;
        }

        if is_in_env() {
            self.upload_native_image_arg_files().await?;

            // If we were running any benchmarks, they are complete by now. Upload the report.
            for bench in &self.config.execute_benchmarks {
                match bench {
                    Benchmarks::Runtime => {
                        let runtime_bench_report = &self
                            .paths
                            .repo_root
                            .engine
                            .join("runtime-benchmarks")
                            .join("bench-report.xml");
                        if runtime_bench_report.exists() {
                            ide_ci::actions::artifacts::upload_single_file(
                                runtime_bench_report,
                                "Runtime Benchmark Report",
                            )
                            .await?;
                        } else {
                            warn!(
                                "No Runtime Benchmark Report file found at {}, nothing to upload.",
                                runtime_bench_report.display()
                            );
                        }
                    }
                    Benchmarks::EnsoJMH => {
                        let enso_jmh_report =
                            &self.paths.repo_root.std_bits.benchmarks.bench_report_xml;
                        if enso_jmh_report.exists() {
                            ide_ci::actions::artifacts::upload_single_file(
                                enso_jmh_report,
                                "Enso JMH Benchmark Report",
                            )
                            .await?;
                        } else {
                            warn!(
                                "No Enso JMH Benchmark Report file found at {}, nothing to upload.",
                                enso_jmh_report.display()
                            );
                        }
                    }
                    _ => {}
                }
            }
        }


        // === Build Distribution ===
        debug!("Building distribution");
        if self.config.build_native_runner {
            debug!("Building and testing native engine runners");
            runner_sanity_test(&self.repo_root, None).await?;
            let enso = self
                .repo_root
                .built_distribution
                .enso_engine_triple
                .engine_package
                .bin
                .join("enso")
                .with_executable_extension();
            ide_ci::fs::remove_file_if_exists(&enso)?;
            if self.config.build_espresso_runner {
                let enso_java = "espresso";
                sbt.command()?
                    .env(ENSO_JAVA, enso_java)
                    .arg("engine-runner/buildNativeImage")
                    .run_ok()
                    .await?;
                runner_sanity_test(&self.repo_root, Some(enso_java)).await?;
            }
        }

        // Verify Integrity of Generated License Packages in Distributions
        // FIXME apparently this does not work on Windows due to some CRLF issues?
        if self.config.verify_packages && TARGET_OS != OS::Windows {
            for package in ret.packages() {
                package.verify_package_sbt(&sbt).await?;
            }
            if self.config.build_engine_package {
                for libname in ["Base", "Table", "Image", "Database"] {
                    let lib_path = self
                        .repo_root
                        .built_distribution
                        .enso_engine_triple
                        .engine_package
                        .lib
                        .join_iter(["Standard", libname])
                        .join(self.paths.version().to_string());
                    sbt.verify_generated_package(libname, lib_path).await?;
                }
            }
        }

        if self.config.build_engine_package {
            if TARGET_OS == OS::Linux && ide_ci::ci::run_in_ci() {
                self.paths.upload_edition_file_artifact().await?;
            }

            let schema_dir = self.paths.repo_root.join_iter([
                "engine",
                "language-server",
                "src",
                "main",
                "schema",
            ]);
            if is_in_env() {
                ide_ci::actions::artifacts::upload_compressed_directory(&schema_dir, "fbs-schema")
                    .await?;
            }
        }

        let graal_version = engine::deduce_graal_bundle(&self.repo_root.build_sbt).await?;
        for bundle in ret.bundles() {
            bundle.create(&self.repo_root, &graal_version).await?;
        }

        scala_test_result?;

        Ok(ret)
    }

    pub async fn execute(&self, operation: Operation) -> Result {
        match operation {
            Operation::Release(ReleaseOperation { command, repo }) => match command {
                ReleaseCommand::Upload => {
                    let artifacts = self.build().await?;
                    let release_id = crate::env::ENSO_RELEASE_ID.get()?;
                    let release = ide_ci::github::release::Handle::new(
                        &self.inner.octocrab,
                        repo,
                        release_id,
                    );
                    for package in artifacts.packages() {
                        package.upload_as_asset(release.clone()).await?;
                    }
                    for bundle in artifacts.bundles() {
                        bundle.upload_as_asset(release.clone()).await?;
                    }
                    if TARGET_OS == OS::Linux {
                        release.upload_asset_file(self.paths.manifest_file()).await?;
                        release.upload_asset_file(self.paths.launcher_manifest_file()).await?;
                    }
                }
            },
            Operation::Run(run) => {
                // Build environment preparations.
                self.prepare_build_env().await?;
                let mut run = run.command_pieces.iter();
                if let Some(program) = run.next() {
                    debug!("Resolving program: {}", program.as_str());
                    let exe_path = ide_ci::program::lookup(program.as_str())?;
                    Command::new(exe_path)
                        .args(run)
                        .current_dir(&self.paths.repo_root)
                        .spawn()?
                        .wait_ok()
                        .await?;
                } else {
                    debug!("Spawning default shell.");
                    let mut shell =
                        DEFAULT_SHELL.run_shell()?.current_dir(&self.paths.repo_root).spawn()?;
                    shell.wait_ok().await?;
                }
            }
            Operation::Sbt(args) => {
                self.prepare_build_env().await?;
                let sbt = engine::sbt::Context {
                    repo_root:         self.paths.repo_root.path.clone(),
                    system_properties: default(),
                };
                sbt.call_args(args).await?;
            }
            Operation::Build => {
                self.build().boxed().await?;
            }
        };

        Ok(())
    }
}

/// Upload the directory with Enso-generated test results.
///
/// This is meant to ease debugging, it does not really affect the build.
#[context("Failed to upload test results.")]
pub async fn upload_test_results(test_results_dir: PathBuf) -> Result {
    // Each platform gets its own log results, so we need to generate unique
    // names.
    let name = format!("Test_Results_{TARGET_OS}");
    let upload_result =
        ide_ci::actions::artifacts::upload_compressed_directory(&test_results_dir, name).await;
    if let Err(err) = &upload_result {
        // We wouldn't want to fail the whole build if we can't upload the test
        // results. Still, it should be somehow
        // visible in the build summary.
        ide_ci::actions::workflow::message(
            MessageLevel::Warning,
            format!("Failed to upload test results: {err}"),
        );
    }
    upload_result
}

/// Run the native runner and check if it produces the expected output on a simple test.
pub async fn runner_sanity_test(
    repo_root: &crate::paths::generated::RepoRoot,
    enso_java: Option<&str>,
) -> Result {
    let engine_package = repo_root.built_distribution.enso_engine_triple.engine_package.as_path();
    // The engine package is necessary for running the native runner.
    ide_ci::fs::tokio::require_exist(engine_package).await?;
    if enso_java.is_none() {
        let enso = repo_root
            .built_distribution
            .enso_engine_triple
            .engine_package
            .bin
            .join("enso")
            .with_executable_extension();
        let test_base = Command::new(&enso)
            .args(["--run", repo_root.test.join("Base_Tests").as_str()])
            .set_env(ENSO_DATA_DIRECTORY, engine_package)?
            .run_ok()
            .await;

        let test_geo = Command::new(&enso)
            .args(["--run", repo_root.test.join("Geo_Tests").as_str()])
            .set_env(ENSO_DATA_DIRECTORY, engine_package)?
            .run_ok()
            .await;

        test_base.and(test_geo)
    } else {
        Ok(())
    }
}
