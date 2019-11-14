# octbook

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

## Usage

## Build

### Docker

```
$ stack --docker build -j 1 Cabal # if out of memory in docker
$ stack --docker --local-bin-path=./bin install
$ docker build -t matsubara0507/octbook . --build-arg local_bin_path=./bin
```
