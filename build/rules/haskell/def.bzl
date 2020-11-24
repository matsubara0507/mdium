load("@rules_haskell//haskell:defs.bzl", "haskell_library")

def _generate_paths_module_impl(ctx):
    paths_file = ctx.actions.declare_file(ctx.label.name)
    ctx.actions.expand_template(
        template = ctx.file._template,
        output = paths_file,
        substitutions = {
            "%{module_name}": ctx.attr.module,
            "%{version}": str(ctx.attr.version.replace(".", ",")),
        },
    )
    return struct(files = depset([paths_file]))

_generate_paths_module = rule(
    implementation = _generate_paths_module_impl,
    attrs = {
        "module": attr.string(),
        "version": attr.string(),
        "_template": attr.label(
            default = ":Paths_module.hs",
            allow_single_file = True,
        ),
    },
)

def paths_module(name, package, version, dir = "gen_paths", deps = ["@stackage//:base"], **kwargs):
    module_name = "Paths_" + package.replace("-", "_")
    paths_file = dir + "/" + module_name + ".hs"
    _generate_paths_module(
        name = paths_file,
        module = module_name,
        version = version,
    )
    haskell_library(
        name = name,
        srcs = [paths_file],
        deps = deps,
        **kwargs,
    )
