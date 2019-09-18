# mdium

## Requirement

- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/) or Docker
- Medium integration token
    - you can generate in [medium settigns](https://medium.com/me/settings).

## Install

Clone this repository:

```
$ git clone https://github.com/matsubara0507/mdium.git
$ cd mdium
```

Install with stack tool:

```
$ stack install mdium
```

### Install with Docker

Use https://hub.docker.com/r/matsubara0507/mdium/

```
$ docker pull matsubara0507/mdium
```

Use command:

```
$ docker run --rm --env-file="$HOME/.env" -v `pwd`:/work matsubara0507/mdium /bin/bash -c "cd work && mdium example/example.md"
```

## Usage

```
$ mdium --help
mdium [options] [input-file]
      --help                Show this help text
      --version             Show version
  -v  --verbose             Enable verbose mode: verbosity level "debug"
      --me                  Call Medium `me` API
      --title=TEXT          Specify title of story that post to medium
      --org=PUBLICATION_ID  Post to override story of PUBLICATION_ID
      --publications        Call Medium `publications` API
```

Set Medium integration token to `MEDIUM_TOKEN` environment (use `./.env` or `~/.env`).
Can check to validate your token by executing `medium --me` command:

```
$ mdium --me
Hi MATSUBARA Nobutada!!
```

If post [`examle/example.md`](example/example.md) to Medium:

```
$ mdium ./example/examle.md
post success, browse to: https://medium.com/@nobutada/e31f70013b36
```

then generate story like below in Medium:

![](example/example.png)

## For Development

### Build with Docker

```
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/mdium . --build-arg local_bin_path=./bin
```
