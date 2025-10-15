"""
Run sbt. Implementation mostly copied from https://github.com/bazel-contrib/bazel-lib/blob/main/docs/run_binary.md.
"""

load("@aspect_bazel_lib//lib:expand_make_vars.bzl", "expand_variables")
load("@aspect_bazel_lib//lib:strings.bzl", "split_args")
load("@bazel_skylib//lib:dicts.bzl", "dicts")
load("@rules_java//java/common:java_common.bzl", "java_common")
load("@bazel_skylib//rules:select_file.bzl", "select_file")
load("@bazel_skylib//rules/directory:directory.bzl", "directory")
load("@bazel_skylib//rules/directory:glob.bzl", "directory_glob")
load("@bazel_skylib//rules/directory:providers.bzl", "DirectoryInfo")

def _run_sbt_impl(ctx):
    sbt_bin = ctx.toolchains["@//toolchains/sbt:toolchain_type"].sbt_info.sbt_bin
    java_runtime = ctx.attr._java_runtime
    java_executable_path = java_runtime[java_common.JavaRuntimeInfo].java_executable_exec_path
    cc_toolchain = ctx.toolchains["@bazel_tools//tools/cpp:toolchain_type"]
    cc_path = cc_toolchain.cc.compiler_executable
    cc_deps = cc_toolchain.cc.all_files

    out_dir = ctx.actions.declare_directory(ctx.attr.out_dir)

    real_executable_path = out_dir.path + "/enso-engine-0.0.0-dev-macos-aarch64/enso-0.0.0-dev/bin/enso"
    exec_out = ctx.actions.declare_file(real_executable_path)
    print("exec_out:", exec_out)

    outputs = [out_dir, exec_out]

    args = ctx.actions.args()
    for a in ctx.attr.args:
        args.add_all(split_args(expand_variables(ctx, ctx.expand_location(a, targets = ctx.attr.srcs), outs = outputs)))
    envs = {}
    for k, v in ctx.attr.env.items():
        envs[k] = expand_variables(ctx, ctx.expand_location(v, targets = ctx.attr.srcs), outs = outputs, attribute_name = "env")

    inputs = depset(ctx.files.srcs, transitive = [java_runtime.files, cc_deps])
    system_props = [
        "-Denso.BazelSupport.outDir=" + out_dir.path,
        "-Denso.BazelSupport.CCompilerPath=" + cc_path,
    ]
    for p in ctx.attr.system_props:
        system_props = system_props + split_args(expand_variables(ctx, ctx.expand_location(p, targets = ctx.attr.srcs), outs = outputs))

    ctx.actions.run(
        outputs = [out_dir, exec_out],
        inputs = inputs,
        executable = java_executable_path,
        arguments = system_props + ["-jar", sbt_bin, args],
        use_default_shell_env = ctx.attr.use_default_shell_env,
        env = dicts.add(ctx.configuration.default_shell_env, envs),
    )
    return DefaultInfo(
        files = depset([out_dir]),
        runfiles = ctx.runfiles(files = [out_dir]),
        executable = exec_out
    )

run_sbt = rule(
    implementation = _run_sbt_impl,
    toolchains = [
        "@//toolchains/sbt:toolchain_type",
        "@//toolchains/flatc:toolchain_type",
        "@bazel_tools//tools/jdk:runtime_toolchain_type",
        "@bazel_tools//tools/cpp:toolchain_type"
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
    },
)


def _enso_binary_impl(ctx):
    dep_files = ctx.attr.run_sbt_dep[DefaultInfo].files_to_run
    exec = dep_files.executable

    print("exec:", exec)

    ctx.actions.run(
        executable = exec,
        arguments = ["--version"]
    )

    return DefaultInfo(

    )


enso_binary = rule(
    implementation = _enso_binary_impl,
    attrs = {
        "run_sbt_dep": attr.label(
            mandatory = True,
            providers = [DefaultInfo]
        )
    },
    executable = True
)
