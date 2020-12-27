# Advent of Code 2020

Solutions to [Advent of Code 2020](https://adventofcode.com/).

## Run a problem

To run the solution for Day 1, execute

```sh
stack run 1
```

## Run all tests

```sh
stack test
```

## Run tests for specific problem

```sh
stack test --ta '-p "Day 24"'
```

## Download problem input

Set the env var `ADVENT_SESSION_COOKIE` to your cookie for `adventofcode.com`. Then execute

```sh
export ADVENT_SESSION_COOKIE=session=abcd
./bin/download_input.sh 1
```

to download the input for Day 1.
