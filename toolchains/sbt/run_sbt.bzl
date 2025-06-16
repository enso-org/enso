load("@aspect_bazel_lib//lib:strings.bzl", "split_args")
load("@aspect_bazel_lib//lib/private:expand_variables.bzl", "expand_variables")
load("@bazel_skylib//lib:dicts.bzl", "dicts")

def _run_sbt_impl(ctx):
    sbt_bin = ctx.toolchains["@//toolchains/sbt:toolchain_type"].sbt_info.sbt_bin
    java_runtime = ctx.attr._java_runtime
    java_executable_path = java_runtime[java_common.JavaRuntimeInfo].java_executable_exec_path
    args = ctx.actions.args()

    outputs = []
    outputs.extend(ctx.outputs.outs)
    for _out_dir in ctx.attr.out_dirs:
        out_dir = ctx.actions.declare_directory(_out_dir)
        for output in outputs:
            if output.path.startswith(out_dir.path + "/"):
                fail("output {} is nested within output directory {}; outputs cannot be nested within each other!".format(output.path, out_dir.path))
            if output.is_directory and out_dir.path.startswith(output.path + "/"):
                fail("output directory {} is nested within output directory {}; outputs cannot be nested within each other!".format(out_dir.path, output.path))
        outputs.append(out_dir)
    if len(outputs) < 1:
        fail("""\
ERROR: target {target} is not configured to produce any outputs.

Bazel only executes actions when their outputs are required, so it's never correct to create an action with no outputs.
""".format(
            target = str(ctx.label),
        ))

    for a in ctx.attr.args:
        args.add_all(split_args(expand_variables(ctx, ctx.expand_location(a, targets = ctx.attr.srcs), outs = outputs)))
    envs = {}
    for k, v in ctx.attr.env.items():
        envs[k] = expand_variables(ctx, ctx.expand_location(v, targets = ctx.attr.srcs), outs = outputs, attribute_name = "env")

    inputs = depset(ctx.files.srcs, transitive = [java_runtime.files])
    system_props = []
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
    ],
    attrs = {
        "args": attr.string_list(
            default = [],
        ),
        "env": attr.string_dict(),
        "srcs": attr.label_list(
            allow_files = True,
        ),
        "system_props": attr.string_list(
            default = [],
        ),
        "out_dirs": attr.string_list(),
        "outs": attr.output_list(),
        "use_default_shell_env": attr.bool(),
        "_java_runtime": attr.label(default = Label("@bazel_tools//tools/jdk:current_java_runtime")),
    },
)
