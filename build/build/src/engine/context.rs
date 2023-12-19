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
use crate::engine::FLATC_VERSION;
use crate::engine::PARALLEL_ENSO_TESTS;
use crate::enso::BenchmarkOptions;
use crate::enso::BuiltEnso;
use crate::enso::IrCaches;
use crate::paths::cache_directory;
use crate::paths::Paths;
use crate::paths::TargetTriple;
use crate::paths::ENSO_TEST_JUNIT_DIR;
use crate::project::ProcessWrapper;

use ide_ci::actions::workflow::is_in_env;
use ide_ci::actions::workflow::MessageLevel;
use ide_ci::cache;
use ide_ci::github::release::IsReleaseExt;
use ide_ci::platform::DEFAULT_SHELL;
use ide_ci::programs::sbt;
use ide_ci::programs::Flatc;
use ide_ci::programs::Sbt;
use std::env::consts::DLL_EXTENSION;
use std::env::consts::EXE_EXTENSION;
use sysinfo::SystemExt;



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

#[derive(derive_more::Deref, derive_more::DerefMut, derivative::Derivative)]
#[derivative(Debug)]
pub struct RunContext {
    #[deref]
    #[deref_mut]
    pub inner:            crate::project::Context,
    pub config:           BuildConfigurationResolved,
    pub paths:            Paths,
    /// If set, the engine package (used for creating bundles) will be obtained through this
    /// provider rather than built from source along the other Engine components.
    #[derivative(Debug(format_with = "format_option_variant"))]
    pub external_runtime: Option<Arc<EnginePackageProvider>>,
}

impl RunContext {
    pub fn new(
        inner: crate::project::Context,
        config: impl Into<BuildConfigurationResolved>,
        triple: TargetTriple,
        external_runtime: Option<Arc<EnginePackageProvider>>,
    ) -> Result<Self> {
        let paths = crate::paths::Paths::new_versions(&inner.repo_root, triple.versions)?;
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
            if self.config.test_scala {
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

        // Setup Conda Environment
        // Install FlatBuffers Compiler
        // If it is not available, we require conda to install it. We should not require conda in
        // other scenarios.
        // TODO: After flatc version is bumped, it should be possible to get it without `conda`.
        //       See: https://www.pivotaltracker.com/story/show/180303547
        if let Err(e) = Flatc.require_present_at(&FLATC_VERSION).await {
            warn!("Cannot find expected flatc: {}", e);
            warn!("Will try to install it using conda. In case of issues, please install flatc manually, to avoid dependency on conda.");
            // GitHub-hosted runner has `conda` on PATH but not things installed by it.
            // It provides `CONDA` variable pointing to the relevant location.
            if let Some(conda_path) = std::env::var_os("CONDA").map(PathBuf::from) {
                ide_ci::env::prepend_to_path(conda_path.join("bin"))?;
                if TARGET_OS == OS::Windows {
                    // Not sure if it documented anywhere, but this is where installed `flatc`
                    // appears on Windows.
                    ide_ci::env::prepend_to_path(conda_path.join("Library").join("bin"))?;
                }
            }

            ide_ci::programs::Conda
                .cmd()?
                .args(["install", "-y", "--freeze-installed", "flatbuffers=1.12.0"])
                .run_ok()
                .await?;
            Flatc.lookup()?;
        }

        self.paths.emit_env_to_actions().await?; // Ignore error: we might not be run on CI.
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

        prepare_simple_library_server.await??;
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
            ide_ci::fs::remove_if_exists(&self.paths.repo_root.engine.runtime.bench_report_xml)?;
        }


        let _test_results_upload_guard =
            if self.config.test_scala || self.config.test_standard_library {
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
        let github_hosted_macos_memory = 15_032_385;
        let big_memory_machine = system.total_memory() > github_hosted_macos_memory;
        // Windows native runner is not yet supported.
        let build_native_runner =
            self.config.build_engine_package() && big_memory_machine && TARGET_OS != OS::Windows;


        // === Build project-manager distribution and native image ===
        debug!("Bulding project-manager distribution and Native Image");
        if big_memory_machine {
            let mut tasks = vec![];

            if self.config.build_engine_package() {
                tasks.push("buildEngineDistribution");
                tasks.push("engine-runner/assembly");
            }
            if build_native_runner {
                tasks.push("engine-runner/buildNativeImage");
            }

            if TARGET_OS != OS::Windows {
                // FIXME [mwu] apparently this is broken on Windows because of the line endings
                // mismatch
                tasks.push("verifyLicensePackages");
            }

            if self.config.build_project_manager_package() {
                tasks.push("buildProjectManagerDistribution");
            }

            if self.config.build_launcher_package() {
                tasks.push("buildLauncherDistribution");
            }
            sbt.call_arg(Sbt::concurrent_tasks(tasks)).await?;
        } else {
            // If we are run on a weak machine (like GH-hosted runner), we need to build things one
            // by one.
            sbt.call_arg("compile").await?;

            // Build the Runner & Runtime Uberjars
            sbt.call_arg("engine-runner/assembly").await?;

            // Build the Launcher Native Image
            sbt.call_arg("launcher/assembly").await?;
            sbt.call_args(&["--mem", "1536", "launcher/buildNativeImage"]).await?;

            // Build the PM Native Image
            sbt.call_arg("project-manager/assembly").await?;
            sbt.call_args(&["--mem", "1536", "project-manager/buildNativeImage"]).await?;

            // Prepare Launcher Distribution
            //create_launcher_package(&paths)?;
            sbt.call_arg("buildLauncherDistribution").await?;

            // Prepare Engine Distribution
            sbt.call_arg("buildEngineDistribution").await?;

            // Prepare Project Manager Distribution
            sbt.call_arg("buildProjectManagerDistribution").await?;
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
                let binaries = binary_extensions
                    .into_iter()
                    .map(|extension| {
                        let pattern = package_dir.join_iter(["**", "*"]).with_extension(extension);
                        glob::glob(pattern.as_str())?.try_collect_vec()
                    })
                    .try_collect_vec()?
                    .into_iter()
                    .flatten()
                    .collect_vec();

                debug!(?binaries, "Found executables in the package.");
                for binary in binaries {
                    ide_ci::packaging::add_msvc_redist_dependencies(&binary).await?;
                }
            }
        }

        let enso = BuiltEnso { paths: self.paths.clone() };

        // === Unit tests and Enso tests ===
        debug!("Running unit tests and Enso tests.");
        if self.config.test_scala {
            // Make sure that `sbt buildEngineDistributionNoIndex` is run before
            // `project-manager/test`. Note that we do not have to run
            // `buildEngineDistribution` (with indexing), because it is unnecessary.
            sbt.call_arg("buildEngineDistributionNoIndex").await?;

            // Run unit tests
            sbt.call_arg("set Global / parallelExecution := false; test").await?;
        }
        if self.config.test_standard_library {
            enso.run_tests(IrCaches::No, &sbt, PARALLEL_ENSO_TESTS).await?;
        }

        perhaps_test_java_generated_from_rust_job.await.transpose()?;

        // === Run benchmarks ===
        debug!("Running benchmarks.");
        if big_memory_machine {
            let mut tasks = vec![];
            // This just compiles benchmarks, not run them. At least we'll know that they can be
            // run. Actually running them, as part of this routine, would be too heavy.
            // TODO [mwu] It should be possible to run them through context config option.
            if self.config.build_benchmarks {
                tasks.extend([
                    "runtime/Benchmark/compile",
                    "language-server/Benchmark/compile",
                    "searcher/Benchmark/compile",
                    "std-benchmarks/Benchmark/compile",
                ]);
            }

            let build_command = (!tasks.is_empty()).then_some(Sbt::concurrent_tasks(tasks));

            // We want benchmarks to run only after the other build tasks are done, as they are
            // really CPU-heavy.
            let benchmark_tasks = self.config.execute_benchmarks.iter().flat_map(|b| b.sbt_task());
            let command_sequence = build_command.as_deref().into_iter().chain(benchmark_tasks);
            let final_command = Sbt::sequential_tasks(command_sequence);
            if !final_command.is_empty() {
                sbt.call_arg(final_command).await?;
            } else {
                debug!("No SBT tasks to run.");
            }
        } else {
            if self.config.build_benchmarks {
                // Check Runtime Benchmark Compilation
                sbt.call_arg("runtime/Benchmark/compile").await?;

                // Check Language Server Benchmark Compilation
                sbt.call_arg("language-server/Benchmark/compile").await?;

                // Check Searcher Benchmark Compilation
                sbt.call_arg("searcher/Benchmark/compile").await?;

                // Check Enso JMH benchmark compilation
                sbt.call_arg("std-benchmarks/Benchmark/compile").await?;
            }

            for benchmark in &self.config.execute_benchmarks {
                if let Some(task) = benchmark.sbt_task() {
                    sbt.call_arg(task).await?;
                }
            }
        }

        if self.config.execute_benchmarks.contains(&Benchmarks::Enso) {
            enso.run_benchmarks(BenchmarkOptions { dry_run: false }).await?;
        } else if self.config.check_enso_benchmarks {
            enso.run_benchmarks(BenchmarkOptions { dry_run: true }).await?;
        }

        // If we were running any benchmarks, they are complete by now. Upload the report.
        if is_in_env() {
            for bench in &self.config.execute_benchmarks {
                match bench {
                    Benchmarks::Runtime => {
                        let runtime_bench_report =
                            &self.paths.repo_root.engine.runtime.bench_report_xml;
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
        if self.config.build_engine_package() {
            let std_libs =
                &self.repo_root.built_distribution.enso_engine_triple.engine_package.lib.standard;
            // let std_libs = self.paths.engine.dir.join("lib").join("Standard");
            // Compile the Standard Libraries (Unix)
            debug!("Compiling standard libraries under {}", std_libs.display());
            for entry in ide_ci::fs::read_dir(std_libs)? {
                let entry = entry?;
                let target = entry.path().join(self.paths.version().to_string());
                enso.compile_lib(target)?.run_ok().await?;
            }
        }


        // if build_native_runner {
        //     let factorial_input = "6";
        //     let factorial_expected_output = "720";
        //     let output = Command::new(&self.repo_root.runner)
        //         .args([
        //             "--run",
        //
        // self.repo_root.engine.runner_native.src.test.resources.factorial_enso.as_str(),
        //             factorial_input,
        //         ])
        //         .env(ENSO_DATA_DIRECTORY.name(), &self.paths.engine.dir)
        //         .run_stdout()
        //         .await?;
        //     ensure!(
        //         output.contains(factorial_expected_output),
        //         "Native runner output does not contain expected result."
        //     );
        // }


        // Verify License Packages in Distributions
        // FIXME apparently this does not work on Windows due to some CRLF issues?
        if self.config.verify_packages && TARGET_OS != OS::Windows {
            /*  refversion=${{ env.ENSO_VERSION }}
                binversion=${{ env.DIST_VERSION }}
                engineversion=$(${{ env.ENGINE_DIST_DIR }}/bin/enso --version --json | jq -r '.version')
                test $binversion = $refversion || (echo "Tag version $refversion and the launcher version $binversion do not match" && false)
                test $engineversion = $refversion || (echo "Tag version $refversion and the engine version $engineversion do not match" && false)
            */

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

        let graal_version = crate::engine::deduce_graal_bundle(&self.repo_root.build_sbt).await?;
        for bundle in ret.bundles() {
            bundle.create(&self.repo_root, &graal_version).await?;
        }

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
                    ide_ci::program::Command::new(exe_path)
                        .args(run)
                        .current_dir(&self.paths.repo_root)
                        .spawn()?
                        .wait()
                        .await?
                        .exit_ok()?;
                } else {
                    debug!("Spawning default shell.");
                    let mut shell =
                        DEFAULT_SHELL.run_shell()?.current_dir(&self.paths.repo_root).spawn()?;
                    shell.wait_ok().await?;
                }
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
