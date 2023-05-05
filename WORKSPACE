# Give your project a name. :)
workspace(name = "mdium")

# Load the repository rule to download an http archive.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

# Download rules_haskell and make it accessible as "@rules_haskell".
http_archive(
    name = "rules_haskell",
    sha256 = "2a07b55c30e526c07138c717b0343a07649e27008a873f2508ffab3074f3d4f3",
    strip_prefix = "rules_haskell-0.16",
    url = "https://github.com/tweag/rules_haskell/archive/refs/tags/v0.16.tar.gz",
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
    "stack_snapshot",
)

stack_snapshot(
    name = "stackage",
    packages = [
        "Cabal",
        "aeson",
        "base",
        "dotenv",
        "extensible",
        "fallible",
        "github",
        "mix",
        "mix-json-logger",
        "mix-plugin-github",
        "pandoc",
        "pandoc-types",
        "rio",
        "wreq",
    ],
    setup_deps = {
        "Glob": ["@stackage//:Cabal"],
        "HUnit": ["@stackage//:Cabal"],
        "HsYAML": ["@stackage//:Cabal"],
        "JuicyPixels": ["@stackage//:Cabal"],
        "OneTuple": ["@stackage//:Cabal"],
        "Only": ["@stackage//:Cabal"],
        "QuickCheck": ["@stackage//:Cabal"],
        "RSA": ["@stackage//:Cabal"],
        "SHA": ["@stackage//:Cabal"],
        "StateVar": ["@stackage//:Cabal"],
        "adjunctions": ["@stackage//:Cabal"],
        "aeson": ["@stackage//:Cabal"],
        "aeson-pretty": ["@stackage//:Cabal"],
        "ansi-terminal": ["@stackage//:Cabal"],
        "ansi-wl-pprint": ["@stackage//:Cabal"],
        "appar": ["@stackage//:Cabal"],
        "array": ["@stackage//:Cabal"],
        "asn1-encoding": ["@stackage//:Cabal"],
        "asn1-parse": ["@stackage//:Cabal"],
        "asn1-types": ["@stackage//:Cabal"],
        "assoc": ["@stackage//:Cabal"],
        "async": ["@stackage//:Cabal"],
        "attoparsec": ["@stackage//:Cabal"],
        "attoparsec-iso8601": ["@stackage//:Cabal"],
        "authenticate-oauth": ["@stackage//:Cabal"],
        "auto-update": ["@stackage//:Cabal"],
        "base": ["@stackage//:Cabal"],
        "base-compat": ["@stackage//:Cabal"],
        "base-compat-batteries": ["@stackage//:Cabal"],
        "base-orphans": ["@stackage//:Cabal"],
        "base16-bytestring": ["@stackage//:Cabal"],
        "base64": ["@stackage//:Cabal"],
        "base64-bytestring": ["@stackage//:Cabal"],
        "basement": ["@stackage//:Cabal"],
        "bifunctors": ["@stackage//:Cabal"],
        "binary": ["@stackage//:Cabal"],
        "binary-instances": ["@stackage//:Cabal"],
        "binary-orphans": ["@stackage//:Cabal"],
        "blaze-builder": ["@stackage//:Cabal"],
        "blaze-html": ["@stackage//:Cabal"],
        "blaze-markup": ["@stackage//:Cabal"],
        "boring": ["@stackage//:Cabal"],
        "bsb-http-chunked": ["@stackage//:Cabal"],
        "byteorder": ["@stackage//:Cabal"],
        "bytestring": ["@stackage//:Cabal"],
        "cabal-doctest": ["@stackage//:Cabal"],
        "call-stack": ["@stackage//:Cabal"],
        "case-insensitive": ["@stackage//:Cabal"],
        "cassava": ["@stackage//:Cabal"],
        "cereal": ["@stackage//:Cabal"],
        "citeproc": ["@stackage//:Cabal"],
        "cmdargs": ["@stackage//:Cabal"],
        "colour": ["@stackage//:Cabal"],
        "commonmark": ["@stackage//:Cabal"],
        "commonmark-extensions": ["@stackage//:Cabal"],
        "commonmark-pandoc": ["@stackage//:Cabal"],
        "comonad": ["@stackage//:Cabal"],
        "conduit": ["@stackage//:Cabal"],
        "conduit-extra": ["@stackage//:Cabal"],
        "connection": ["@stackage//:Cabal"],
        "constraints": ["@stackage//:Cabal"],
        "containers": ["@stackage//:Cabal"],
        "contravariant": ["@stackage//:Cabal"],
        "cookie": ["@stackage//:Cabal"],
        "crypto-api": ["@stackage//:Cabal"],
        "crypto-pubkey-types": ["@stackage//:Cabal"],
        "cryptohash-sha1": ["@stackage//:Cabal"],
        "cryptonite": ["@stackage//:Cabal"],
        "data-array-byte": ["@stackage//:Cabal"],
        "data-default": ["@stackage//:Cabal"],
        "data-default-class": ["@stackage//:Cabal"],
        "data-default-instances-containers": ["@stackage//:Cabal"],
        "data-default-instances-dlist": ["@stackage//:Cabal"],
        "data-default-instances-old-locale": ["@stackage//:Cabal"],
        "data-fix": ["@stackage//:Cabal"],
        "dec": ["@stackage//:Cabal"],
        "deepseq": ["@stackage//:Cabal"],
        "deepseq-generics": ["@stackage//:Cabal"],
        "digest": ["@stackage//:Cabal"],
        "directory": ["@stackage//:Cabal"],
        "distributive": ["@stackage//:Cabal"],
        "dlist": ["@stackage//:Cabal"],
        "doclayout": ["@stackage//:Cabal"],
        "doctemplates": ["@stackage//:Cabal"],
        "dotenv": ["@stackage//:Cabal"],
        "easy-file": ["@stackage//:Cabal"],
        "emojis": ["@stackage//:Cabal"],
        "entropy": ["@stackage//:Cabal"],
        "errors": ["@stackage//:Cabal"],
        "exceptions": ["@stackage//:Cabal"],
        "extensible": ["@stackage//:Cabal"],
        "fallible": ["@stackage//:Cabal"],
        "fast-logger": ["@stackage//:Cabal"],
        "file-embed": ["@stackage//:Cabal"],
        "filepath": ["@stackage//:Cabal"],
        "free": ["@stackage//:Cabal"],
        "ghc-bignum": ["@stackage//:Cabal"],
        "ghc-boot-th": ["@stackage//:Cabal"],
        "ghc-prim": ["@stackage//:Cabal"],
        "github": ["@stackage//:Cabal"],
        "gridtables": ["@stackage//:Cabal"],
        "haddock-library": ["@stackage//:Cabal"],
        "happy": ["@stackage//:Cabal"],
        "hashable": ["@stackage//:Cabal"],
        "haskell-lexer": ["@stackage//:Cabal"],
        "hourglass": ["@stackage//:Cabal"],
        "hslua": ["@stackage//:Cabal"],
        "hslua-aeson": ["@stackage//:Cabal"],
        "hslua-classes": ["@stackage//:Cabal"],
        "hslua-core": ["@stackage//:Cabal"],
        "hslua-marshalling": ["@stackage//:Cabal"],
        "hslua-module-doclayout": ["@stackage//:Cabal"],
        "hslua-module-path": ["@stackage//:Cabal"],
        "hslua-module-system": ["@stackage//:Cabal"],
        "hslua-module-text": ["@stackage//:Cabal"],
        "hslua-module-version": ["@stackage//:Cabal"],
        "hslua-objectorientation": ["@stackage//:Cabal"],
        "hslua-packaging": ["@stackage//:Cabal"],
        "http-api-data": ["@stackage//:Cabal"],
        "http-client": ["@stackage//:Cabal"],
        "http-client-tls": ["@stackage//:Cabal"],
        "http-date": ["@stackage//:Cabal"],
        "http-link-header": ["@stackage//:Cabal"],
        "http-media": ["@stackage//:Cabal"],
        "http-types": ["@stackage//:Cabal"],
        "http2": ["@stackage//:Cabal"],
        "incremental": ["@stackage//:Cabal"],
        "indexed-traversable": ["@stackage//:Cabal"],
        "indexed-traversable-instances": ["@stackage//:Cabal"],
        "integer-gmp": ["@stackage//:Cabal"],
        "integer-logarithms": ["@stackage//:Cabal"],
        "invariant": ["@stackage//:Cabal"],
        "iproute": ["@stackage//:Cabal"],
        "ipynb": ["@stackage//:Cabal"],
        "iso8601-time": ["@stackage//:Cabal"],
        "jira-wiki-markup": ["@stackage//:Cabal"],
        "kan-extensions": ["@stackage//:Cabal"],
        "lens": ["@stackage//:Cabal"],
        "lens-aeson": ["@stackage//:Cabal"],
        "libyaml": ["@stackage//:Cabal"],
        "lpeg": ["@stackage//:Cabal"],
        "lua": ["@stackage//:Cabal"],
        "mdium": ["@stackage//:Cabal"],
        "megaparsec": ["@stackage//:Cabal"],
        "membership": ["@stackage//:Cabal"],
        "memory": ["@stackage//:Cabal"],
        "microlens": ["@stackage//:Cabal"],
        "microlens-mtl": ["@stackage//:Cabal"],
        "mime-types": ["@stackage//:Cabal"],
        "mix": ["@stackage//:Cabal"],
        "mix-json-logger": ["@stackage//:Cabal"],
        "mix-plugin-github": ["@stackage//:Cabal"],
        "mmorph": ["@stackage//:Cabal"],
        "modern-uri": ["@stackage//:Cabal"],
        "monad-control": ["@stackage//:Cabal"],
        "mono-traversable": ["@stackage//:Cabal"],
        "mtl": ["@stackage//:Cabal"],
        "mtl-compat": ["@stackage//:Cabal"],
        "network": ["@stackage//:Cabal"],
        "network-byte-order": ["@stackage//:Cabal"],
        "network-uri": ["@stackage//:Cabal"],
        "old-locale": ["@stackage//:Cabal"],
        "old-time": ["@stackage//:Cabal"],
        "optparse-applicative": ["@stackage//:Cabal"],
        "pandoc": ["@stackage//:Cabal"],
        "pandoc-lua-marshal": ["@stackage//:Cabal"],
        "pandoc-types": ["@stackage//:Cabal"],
        "parallel": ["@stackage//:Cabal"],
        "parsec": ["@stackage//:Cabal"],
        "parser-combinators": ["@stackage//:Cabal"],
        "pem": ["@stackage//:Cabal"],
        "pretty": ["@stackage//:Cabal"],
        "pretty-show": ["@stackage//:Cabal"],
        "prettyprinter": ["@stackage//:Cabal"],
        "primitive": ["@stackage//:Cabal"],
        "process": ["@stackage//:Cabal"],
        "profunctors": ["@stackage//:Cabal"],
        "psqueues": ["@stackage//:Cabal"],
        "random": ["@stackage//:Cabal"],
        "recv": ["@stackage//:Cabal"],
        "reflection": ["@stackage//:Cabal"],
        "req": ["@stackage//:Cabal"],
        "resourcet": ["@stackage//:Cabal"],
        "retry": ["@stackage//:Cabal"],
        "rio": ["@stackage//:Cabal"],
        "rts": ["@stackage//:Cabal"],
        "safe": ["@stackage//:Cabal"],
        "safe-exceptions": ["@stackage//:Cabal"],
        "scientific": ["@stackage//:Cabal"],
        "semialign": ["@stackage//:Cabal"],
        "semigroupoids": ["@stackage//:Cabal"],
        "semigroups": ["@stackage//:Cabal"],
        "servant": ["@stackage//:Cabal"],
        "servant-server": ["@stackage//:Cabal"],
        "shellwords": ["@stackage//:Cabal"],
        "simple-sendfile": ["@stackage//:Cabal"],
        "singleton-bool": ["@stackage//:Cabal"],
        "skylighting": ["@stackage//:Cabal"],
        "skylighting-core": ["@stackage//:Cabal"],
        "skylighting-format-ansi": ["@stackage//:Cabal"],
        "skylighting-format-blaze-html": ["@stackage//:Cabal"],
        "skylighting-format-context": ["@stackage//:Cabal"],
        "skylighting-format-latex": ["@stackage//:Cabal"],
        "socks": ["@stackage//:Cabal"],
        "some": ["@stackage//:Cabal"],
        "sop-core": ["@stackage//:Cabal"],
        "split": ["@stackage//:Cabal"],
        "splitmix": ["@stackage//:Cabal"],
        "stm": ["@stackage//:Cabal"],
        "streaming-commons": ["@stackage//:Cabal"],
        "strict": ["@stackage//:Cabal"],
        "string-conversions": ["@stackage//:Cabal"],
        "syb": ["@stackage//:Cabal"],
        "tagged": ["@stackage//:Cabal"],
        "tagsoup": ["@stackage//:Cabal"],
        "template-haskell": ["@stackage//:Cabal"],
        "temporary": ["@stackage//:Cabal"],
        "texmath": ["@stackage//:Cabal"],
        "text": ["@stackage//:Cabal"],
        "text-binary": ["@stackage//:Cabal"],
        "text-conversions": ["@stackage//:Cabal"],
        "text-short": ["@stackage//:Cabal"],
        "th-abstraction": ["@stackage//:Cabal"],
        "th-compat": ["@stackage//:Cabal"],
        "th-lift": ["@stackage//:Cabal"],
        "th-lift-instances": ["@stackage//:Cabal"],
        "these": ["@stackage//:Cabal"],
        "time": ["@stackage//:Cabal"],
        "time-compat": ["@stackage//:Cabal"],
        "time-locale-compat": ["@stackage//:Cabal"],
        "time-manager": ["@stackage//:Cabal"],
        "tls": ["@stackage//:Cabal"],
        "transformers": ["@stackage//:Cabal"],
        "transformers-base": ["@stackage//:Cabal"],
        "transformers-compat": ["@stackage//:Cabal"],
        "type-equality": ["@stackage//:Cabal"],
        "typed-process": ["@stackage//:Cabal"],
        "unicode-collation": ["@stackage//:Cabal"],
        "unicode-data": ["@stackage//:Cabal"],
        "unicode-transforms": ["@stackage//:Cabal"],
        "uniplate": ["@stackage//:Cabal"],
        "unix": ["@stackage//:Cabal"],
        "unix-compat": ["@stackage//:Cabal"],
        "unix-time": ["@stackage//:Cabal"],
        "unliftio": ["@stackage//:Cabal"],
        "unliftio-core": ["@stackage//:Cabal"],
        "unordered-containers": ["@stackage//:Cabal"],
        "utf8-string": ["@stackage//:Cabal"],
        "uuid-types": ["@stackage//:Cabal"],
        "vault": ["@stackage//:Cabal"],
        "vector": ["@stackage//:Cabal"],
        "vector-algorithms": ["@stackage//:Cabal"],
        "vector-binary-instances": ["@stackage//:Cabal"],
        "void": ["@stackage//:Cabal"],
        "wai": ["@stackage//:Cabal"],
        "wai-app-static": ["@stackage//:Cabal"],
        "wai-extra": ["@stackage//:Cabal"],
        "wai-logger": ["@stackage//:Cabal"],
        "warp": ["@stackage//:Cabal"],
        "witherable": ["@stackage//:Cabal"],
        "word8": ["@stackage//:Cabal"],
        "wreq": ["@stackage//:Cabal"],
        "x509": ["@stackage//:Cabal"],
        "x509-store": ["@stackage//:Cabal"],
        "x509-system": ["@stackage//:Cabal"],
        "x509-validation": ["@stackage//:Cabal"],
        "xml": ["@stackage//:Cabal"],
        "xml-conduit": ["cabal-doctest", "@stackage//:Cabal"],
        "xml-types": ["@stackage//:Cabal"],
        "yaml": ["@stackage//:Cabal"],
        "zip-archive": ["@stackage//:Cabal"],
        "zlib": ["@stackage//:Cabal"],
    },
    components = {
        "attoparsec": [
            "lib:attoparsec",
            "lib:attoparsec-internal",
        ],
    },
    components_dependencies = {
        "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
    },
    flags = {
        "digest": ["-pkg-config"],
    },
    local_snapshot = "//:stack-snapshot.yaml",
)

# Download a GHC binary distribution from haskell.org and register it as a toolchain.
rules_haskell_toolchains(version = "9.2.5")

http_archive(
    name = "rules_pkg",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.9.1/rules_pkg-0.9.1.tar.gz",
        "https://github.com/bazelbuild/rules_pkg/releases/download/0.9.1/rules_pkg-0.9.1.tar.gz",
    ],
    sha256 = "8f9ee2dc10c1ae514ee599a8b42ed99fa262b757058f65ad3c384289ff70c4b8",
)

load(
    "@rules_pkg//:deps.bzl",
    "rules_pkg_dependencies",
)
rules_pkg_dependencies()

# Docker
http_archive(
    name = "io_bazel_rules_docker",
    sha256 = "b1e80761a8a8243d03ebca8845e9cc1ba6c82ce7c5179ce2b295cd36f7e394bf",
    urls = ["https://github.com/bazelbuild/rules_docker/releases/download/v0.25.0/rules_docker-v0.25.0.tar.gz"],
)

load(
    "@io_bazel_rules_docker//repositories:repositories.bzl",
    container_repositories = "repositories",
)
container_repositories()

load(
    "@io_bazel_rules_docker//repositories:deps.bzl",
    container_deps = "deps",
)
container_deps()

load(
    "@io_bazel_rules_docker//container:container.bzl",
    "container_pull",
)

container_pull(
    name = "haskell_base",
    registry = "ghcr.io",
    repository = "matsubara0507/ubuntu-for-haskell",
    digest = "sha256:054636add6d47411fa2047cae2d33a9efbe570bdc97504b5eeeaee359d79be79",
)
