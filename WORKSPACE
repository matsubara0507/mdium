# Give your project a name. :)
workspace(name = "mdium")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive"
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.13",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.13.tar.gz"],
    sha256 = "b4e2c00da9bc6668fa0404275fecfdb31beb700abdba0e029e74cacc388d94d6",
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup all Bazel dependencies required by rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

load(
    "@rules_haskell//haskell:cabal.bzl",
    "stack_snapshot"
)

stack_snapshot(
    name = "stackage",
    packages = [
        "base",
        "rio",
        "aeson",
        "dotenv",
        "extensible",
        "fallible",
        "mix",
        "mix-json-logger",
        "wreq",
    ],
    # Last snapshot published for ghc-8.6.5 the default version picked up by
    # rules_haskell
    # snapshot = "lts-14.27",
    local_snapshot = "//:stack-snapshot.yaml",
    # This uses an unpinned version of stack_snapshot, meaning that stack is invoked on every build.
    # To switch to pinned stackage dependencies, run `bazel run @stackage-unpinned//:pin` and
    # uncomment the following line.
    # stack_snapshot_json = "//:stackage_snapshot.json",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains()

# Docker
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "4521794f0fba2e20f3bf15846ab5e01d5332e587e9ce81629c7f96c793bb7036",
    strip_prefix = "rules_docker-0.14.4",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.14.4/rules_docker-v0.14.4.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)
container_repositories()

load("@io_bazel_rules_docker//repositories:deps.bzl", container_deps = "deps")

container_deps()

load("@io_bazel_rules_docker//repositories:pip_repositories.bzl", "pip_deps")
pip_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)

container_pull(
    name = "haskell_base",
    registry = "registry.hub.docker.com",
    repository = "matsubara0507/ubuntu-for-haskell",
    digest = "sha256:5967c5908a6c79dc4f4253badfe90326aaf4584a3eaa42d9c9ecc5ae8ba4d133",
)
