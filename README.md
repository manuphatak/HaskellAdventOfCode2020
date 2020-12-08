[![bionikspoon](https://circleci.com/gh/bionikspoon/HaskellAdventOfCode2020.svg?style=svg)](https://circleci.com/gh/bionikspoon/HaskellAdventOfCode2020)
[![codecov](https://codecov.io/gh/bionikspoon/HaskellAdventOfCode2020/branch/main/graph/badge.svg?token=TKOFLWZ1IE)](https://codecov.io/gh/bionikspoon/HaskellAdventOfCode2020)

# AdventOfCode 2020

Solutions to adventofcode.com/2020

## Run tests

```sh
$ stack test --file-watch --fast
```

## Workflow

### Start a new day Part 1

```sh
$ day 1 commit
```

`./bin/day 1 commit` Creates a workspace to solve a new day's problem.

Steps

- copies the Template directories and string replaces placeholder values
- fetches input the problem input and saves it to `./test/Day01/input.txt`
- fetches the problem statement, converts it to markdown, and saves it as
  `./src/Day01/README.md`
- updates the `.cabal` file
- if `commit` is included
  - creates a branch
  - adds and commits the new files
  - pushes the branch to origin
  - opens a pull-request

### Continue to Part 2

```sh
$ readme 1
```

`./bin/readme 1` After solving part 1, get a new README with the full problem
statement including part 2.

## Setup

### Dependencies

- [`direnv`](https://direnv.net/) - used to add project specific env variables
- `yarn global add @bionikspoon/html-to-md`
- `yarn global add prettier"`
- `go get github.com/ericchiang/pup"`
- https://hub.docker.com/ API key for Circle CI
- https://codecov.io/ project token for circle CI

### Getting Started

1. Create an `.envrc`

   ```sh
   $ cp .envrc.sample .envrc
   ```

1. Add a `SESSION_ID` to the `.envrc`

   - go to `https://adventofcode.com/`
   - login
   - Right click -> "Inspect" -> Click the "Application" tab -> Look for
     "Storage -> Cookies -> https://adventofcode.com/" -> Copy the "session"
     value
   - Save the "session" value as `SESSION_ID` in `.envrc`

1. Enable the `.envrc` file

   ```sh
   $ direnv allow
   ```

1. Add environment variables to [CircleCI](https://app.circleci.com/)

   - `CODECOV_TOKEN`
   - `DOCKERHUB_PASSWORD`
   - `DOCKERHUB_USERNAME`

1. Setup `.hspec`

   ```sh
   $ cp .hspec.sample .hspec
   ```
