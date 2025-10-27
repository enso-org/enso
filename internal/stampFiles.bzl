"""
Runs a node.js script with the following arguments:
- input directory, where the files to be stamped are located
- output directory, where the stamped files must be placed
- regexp pattern to filter the files to be stamped
- status file path

If the stamping is globally disabled, the status file path is not provided, and the script should just copy the files to the output directory.
"""

load("@aspect_bazel_lib//lib:expand_make_vars.bzl", "expand_variables")
load("@aspect_bazel_lib//lib:stamping.bzl", "STAMP_ATTRS", "maybe_stamp")

def _stamp_files_impl(ctx):
    out_dir = ctx.actions.declare_directory(ctx.attr.out)
    outputs = [out_dir]
    inputs = [] + ctx.files.srcs
    input_dir = expand_variables(ctx, ctx.expand_location(ctx.attr.input_dir, targets = ctx.attr.srcs), outs = outputs)
    args = ctx.actions.args()
    args.add(input_dir)
    args.add(out_dir.path)
    args.add(ctx.attr.files_regex)
    stamp = maybe_stamp(ctx)
    if stamp:
        args.add(stamp.stable_status_file.path)
        inputs.append(stamp.stable_status_file)

    ctx.actions.run(
        inputs = inputs,
        outputs = outputs,
        arguments = [args],
        env = {
            "BAZEL_BINDIR": ctx.bin_dir.path,
        },
        executable = ctx.executable._stamp_exec,
    )
    return [DefaultInfo(files = depset(outputs), runfiles = ctx.runfiles(files = outputs))]

stamp_files = rule(
    implementation = _stamp_files_impl,
    attrs = dict({
        "srcs": attr.label_list(allow_files = True, mandatory = True, doc = "The files to be stamped."),
        "input_dir": attr.string(mandatory = True, doc = "The path to the input directory, relative to exec root."),
        "files_regex": attr.string(
            mandatory = True,
            doc = """The regexp pattern to filter the files to be stamped.
            If the file contains stamping placeholder, but the filename does not match the pattern, the stamping will fail.
            Example: `config-[0-9a-zA-Z]+\\.js$|index\\.html$`""",
        ),
        "out": attr.string(mandatory = True, doc = "The path to the output directory, relative to exec root."),
        "_stamp_exec": attr.label(executable = True, default = Label("//internal:script_env_replacer"), cfg = "exec"),
    }, **STAMP_ATTRS),
)
