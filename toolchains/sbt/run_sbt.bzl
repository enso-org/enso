load("@aspect_bazel_lib//lib:strings.bzl", "split_args")
load("@aspect_bazel_lib//lib/private:expand_variables.bzl", "expand_variables")
load("@bazel_skylib//lib:dicts.bzl", "dicts")

def _run_sbt_impl(ctx):
    sbt_bin = ctx.toolchains["@//toolchains/sbt:toolchain_type"].sbt_info.sbt_bin
    java_runtime = ctx.attr._java_runtime
    java_executable_path = java_runtime[java_common.JavaRuntimeInfo].java_executable_exec_path

    if len(ctx.attr.out_dir) != 1:
        fail("Exactly one output directory must be specified via the 'out_dir' attribute, but got: {}".format(ctx.attr.out_dir))
    out_dir = ctx.actions.declare_directory(ctx.attr.out_dir[0])
    outputs = [out_dir]

    print("outputs: ", outputs)

    args = ctx.actions.args()
    for a in ctx.attr.args:
        args.add_all(split_args(expand_variables(ctx, ctx.expand_location(a, targets = ctx.attr.srcs), outs = outputs)))
    envs = {}
    for k, v in ctx.attr.env.items():
        envs[k] = expand_variables(ctx, ctx.expand_location(v, targets = ctx.attr.srcs), outs = outputs, attribute_name = "env")

    inputs = depset(ctx.files.srcs, transitive = [java_runtime.files])
    system_props = []
    system_props += [
        "-Denso.BazelSupport.outDir=" + out_dir.path,
    ]
    for p in ctx.attr.system_props:
        system_props = system_props + split_args(expand_variables(ctx, ctx.expand_location(p, targets = ctx.attr.srcs), outs = outputs))

    ctx.actions.run(
        outputs = outputs,
        inputs = inputs,
        executable = java_executable_path,
        arguments = system_props + ["-jar", sbt_bin, args],
        use_default_shell_env = ctx.attr.use_default_shell_env,
        env = dicts.add(ctx.configuration.default_shell_env, envs),
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
        "out_dir": attr.string_list(
            mandatory = True,
            allow_empty = False,
            doc = "Single output directory. sbt will create this directory and put all its outputs there.",
        ),
        "use_default_shell_env": attr.bool(),
        "_java_runtime": attr.label(default = Label("@bazel_tools//tools/jdk:current_java_runtime")),
    },
)
