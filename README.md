# octbook

![](https://github.com/matsubara0507/octbook/workflows/Build/badge.svg)

Invite GitHub Organizations or GitHub Organizations Team by config YAML:

```yaml
- id: matsubara0507
  name: MATSUBARA Nobutada
  org: IGGG
  teams: []
- id: octcat
  name: Octcat
  org: github
  teams:
  - developer
```

## Requirement

Docker or Haskell Stack

## Usage

```
$ octbook --help
octbook [options] [input-file]
  -h  --help               Show this help text
      --version            Show version
  -v  --verbose            Enable verbose mode: verbosity level "debug"
      --users=IDS          Filter users
      --teams=IDS          Filter teams
      --invite=(org|team)  Invite user to GitHub Org or Team
      --kick=(org|team)    Kick user from GitHub Org or Team
```

### Invite Organization

```
$ octbook --invite=org --users=matsubara0507 path/to/.octbook.yaml
```

### Kick Organization

```
$ octbook --kick=org --users=matsubara0507 path/to/.octbook.yaml
```

### Invite Organization Team

```
$ octbook --kick=team --users=matsubara0507 --teams=Sample path/to/.octbook.yaml
```

### Kick Organization Team

```
$ octbook --kick=team --users=matsubara0507 --teams=Sample path/to/.octbook.yaml
```

### Docker

use [matsubara0507/octbook](https://github.com/matsubara0507/octbook/packages)

```
$ docker run -v `pwd`:/work docker.pkg.github.com/matsubara0507/octbook/cli --help
```

## Build

```
$ stack build
```

### Docker

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/octbook . --build-arg local_bin_path=./bin
```

or

```
$ make image tag=matsubara0507/octbook
```
