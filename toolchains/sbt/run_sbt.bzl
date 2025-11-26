"""
Run sbt. Implementation mostly copied from https://github.com/bazel-contrib/bazel-lib/blob/main/docs/run_binary.md.
"""

load("@aspect_bazel_lib//lib:expand_make_vars.bzl", "expand_variables")
load("@aspect_bazel_lib//lib:strings.bzl", "split_args")
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@bazel_skylib//lib:paths.bzl", "paths")
load(
    "@bazel_tools//tools/build_defs/cc:action_names.bzl",
    "CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME",
    "CPP_LINK_EXECUTABLE_ACTION_NAME",
    "CPP_LINK_STATIC_LIBRARY_ACTION_NAME",
    "C_COMPILE_ACTION_NAME",
)
load("@bazel_tools//tools/cpp:toolchain_utils.bzl", "find_cpp_toolchain")
load("@rules_cc//cc/common:cc_common.bzl", "cc_common")
load("@rules_cc//cc/common:cc_info.bzl", "CcInfo")
load("@rules_java//java/common:java_common.bzl", "java_common")

def _run_sbt_impl(ctx):
    sbt_bin = ctx.toolchains["@//toolchains/sbt:toolchain_type"].sbt_info.sbt_bin
    java_runtime = ctx.attr._java_runtime
    java_executable_path = java_runtime[java_common.JavaRuntimeInfo].java_executable_exec_path
    native_toolchain = _resolve_native_toolchain(ctx)
    cc_deps = native_toolchain.transitive_inputs

    out_dir = ctx.actions.declare_directory(ctx.attr.out_dir)
    outputs = [out_dir]

    envs = {}
    for k, v in ctx.attr.env.items():
        envs[k] = expand_variables(ctx, ctx.expand_location(v, targets = ctx.attr.srcs), outs = outputs, attribute_name = "env")

    # Merge user-provided PATH with native toolchain’s PATH.
    user_provided_path = envs["PATH"]
    envs = dicts.add(dicts.omit(envs, ["PATH"]), native_toolchain.env)
    if user_provided_path:
        envs["PATH"] = ctx.configuration.host_path_separator.join([user_provided_path, native_toolchain.env["PATH"]])
    else:
        envs["PATH"] = native_toolchain.env["PATH"]

    system_props = [
        "-Denso.BazelSupport.outDir=" + out_dir.path,
    ]

    direct_inputs = [] + ctx.files.srcs

    # On Linux, we need to add the zlib static library to the linker search path for the native image build.
    if CcInfo in ctx.attr._zlib and ctx.target_platform_has_constraint(ctx.attr._linux_constraint[platform_common.ConstraintValueInfo]):
        linking_context = ctx.attr._zlib[CcInfo].linking_context
        linker_inputs = linking_context.linker_inputs.to_list()
        library = linker_inputs[0].libraries[0]
        static_library = library.pic_static_library
        if not static_library:
            static_library = library.static_library
        zlib_static = ctx.actions.declare_file(ctx.attr.name + "_hermetic_libs/libz.a")
        ctx.actions.symlink(output = zlib_static, target_file = static_library)
        system_props.append("-Denso.BazelSupport.CLibraryPath=" + zlib_static.dirname)
        direct_inputs.append(zlib_static)

    inputs = depset(direct_inputs, transitive = [java_runtime.files, cc_deps])

    for p in ctx.attr.system_props:
        system_props = system_props + split_args(expand_variables(ctx, ctx.expand_location(p, targets = ctx.attr.srcs), outs = outputs))

    args = " ".join(ctx.attr.args)
    ctx.actions.run(
        outputs = outputs,
        inputs = inputs,
        executable = java_executable_path,
        arguments = system_props + ["-jar", sbt_bin] + [args],
        use_default_shell_env = ctx.attr.use_default_shell_env,
        env = dicts.add(ctx.configuration.default_shell_env, envs),
        execution_requirements = {k: "" for k in native_toolchain.execution_requirements},
    )
    return DefaultInfo(
        files = depset(outputs),
        runfiles = ctx.runfiles(files = outputs),
    )

run_sbt = rule(
    implementation = _run_sbt_impl,
    toolchains = [
        "@//toolchains/sbt:toolchain_type",
        "@//toolchains/flatc:toolchain_type",
        "@bazel_tools//tools/jdk:runtime_toolchain_type",
        "@bazel_tools//tools/cpp:toolchain_type",
    ],
    fragments = [
        "cpp",
        "java",
        "platform",
    ],
    attrs = {
        "args": attr.string_list(
            default = [],
            doc = "Arguments for the sbt process",
        ),
        "env": attr.string_dict(
            doc = "Environment variables to set for the sbt process.",
        ),
        "srcs": attr.label_list(
            allow_files = True,
        ),
        "system_props": attr.string_list(
            default = [],
            doc = "Additional system properties to pass to the sbt process. " +
                  "Use the full syntax `-Dproperty=value`",
        ),
        "out_dir": attr.string(
            mandatory = True,
            doc = "Output directory. sbt will create this directory and put all its outputs there.",
        ),
        "use_default_shell_env": attr.bool(),
        "_java_runtime": attr.label(default = Label("@bazel_tools//tools/jdk:current_java_runtime")),
        "_windows_constraint": attr.label(
            default = Label("@platforms//os:windows"),
        ),
        "_linux_constraint": attr.label(
            default = Label("@platforms//os:linux"),
        ),
        "_zlib": attr.label(providers = [[CcInfo]], default = Label("@zlib")),
    },
)

def _resolve_native_toolchain(ctx):
    """Build a context struct for accessing the native C toolchain.

    Available struct properties:
    - `c_compiler_path`: Resolved path to the C compiler which should be used for the native image build.
    - `env`: Environment to use; includes an assembled `PATH` for older rule invocations.
    - `execution_requirements`: Resolved link and compile requirements.
    - `transitive_inputs`: Transitive inputs of cc_toolchain.

    The implementation is mostly copied from https://github.com/sgammon/rules_graalvm/blob/81fc9b2de33c0429b716f1bfcd3223a82c0e1085/internal/native_image/toolchain.bzl#L23
    """

    cc_toolchain = find_cpp_toolchain(ctx)
    is_windows = ctx.target_platform_has_constraint(
        ctx.attr._windows_constraint[platform_common.ConstraintValueInfo],
    )

    feature_configuration = cc_common.configure_features(
        ctx = ctx,
        cc_toolchain = cc_toolchain,
        requested_features = ctx.features,
        unsupported_features = ctx.disabled_features,
    )
    c_compiler_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    )
    ld_executable_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_EXECUTABLE_ACTION_NAME,
    )
    ld_static_lib_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_STATIC_LIBRARY_ACTION_NAME,
    )
    ld_dynamic_lib_path = cc_common.get_tool_for_action(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_DYNAMIC_LIBRARY_ACTION_NAME,
    )
    compile_variables = cc_common.create_compile_variables(
        cc_toolchain = cc_toolchain,
        feature_configuration = feature_configuration,
    )
    compile_env = cc_common.get_environment_variables(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
        variables = compile_variables,
    )
    compile_requirements = cc_common.get_execution_requirements(
        feature_configuration = feature_configuration,
        action_name = C_COMPILE_ACTION_NAME,
    )
    link_variables = cc_common.create_link_variables(
        cc_toolchain = cc_toolchain,
        feature_configuration = feature_configuration,
    )

    # We assume that all link actions use the same environment and execution requirements.
    link_env = cc_common.get_environment_variables(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_EXECUTABLE_ACTION_NAME,
        variables = link_variables,
    )
    link_requirements = cc_common.get_execution_requirements(
        feature_configuration = feature_configuration,
        action_name = CPP_LINK_EXECUTABLE_ACTION_NAME,
    )

    # build final env and execution requirements
    env = dicts.add(compile_env, link_env)
    execution_requirements = compile_requirements + link_requirements

    path_set = {}
    tool_paths = [c_compiler_path, ld_executable_path, ld_static_lib_path, ld_dynamic_lib_path]
    for tool_path in tool_paths:
        tool_dir, _, _ = tool_path.rpartition("/")
        path_set[tool_dir] = None

    paths = sorted(path_set.keys())
    if is_windows:
        # Graal verifies the Visual Studio setup by looking for cl.exe in PATH,
        # which in turn relies on cmd.exe being in PATH.
        # https://github.com/oracle/graal/blob/46de6045d403bb373f65d5e3303f9d3d09f838df/substratevm/src/com.oracle.svm.driver/src/com/oracle/svm/driver/WindowsBuildEnvironmentUtil.java#L130-L135
        paths.append("C:\\Windows\\System32")
    else:
        # The tools returned above may be bash scripts that reference commands
        # in directories we might not otherwise include. For example,
        # on macOS, wrapped_ar calls dirname.
        if "/bin" not in path_set:
            paths.append("/bin")
        if "/usr/bin" not in path_set:
            paths.append("/usr/bin")

    env["PATH"] = ctx.configuration.host_path_separator.join(paths)

    return struct(
        c_compiler_path = c_compiler_path,
        env = env,
        execution_requirements = execution_requirements,
        transitive_inputs = cc_toolchain.all_files,
    )
