#!/usr/bin/env bash

set -euo pipefail

SCRIPTPATH="$(cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P)"
TESTS_BASE="$SCRIPTPATH/../tests"

panic() {
	printf "%s\n" "$1"
	exit 1
}

function clean_builds() {
  echo "Cleaning build artifacts..."
  local dirs=$(find . -name "_build")
  rm -rf ${dirs}
}

function set_erlang_nif_version() {
  IFS='.' read -r -a ERL_NIF_VERSION <<< "$(erl -eval 'io:format("~s", [erlang:system_info(nif_version)]), halt().'  -noshell)"

  ERL_NIF_MAJOR_VERSION=${ERL_NIF_VERSION[0]}
  ERL_NIF_MINOR_VERSION=${ERL_NIF_VERSION[1]}
}

function build_integration_tests() {
  function build_test() {
    local name="$1"

    local test_folder="$TESTS_BASE/integration/$name"
    local build_folder="$test_folder/_build"

    echo "  - 'integration/$name'"

    rm -rf "$build_folder"
    mkdir -p "$build_folder"

    # Build Odin Files
    odin build "$test_folder" \
      -define:ERL_NIF_MAJOR_VERSION=$ERL_NIF_MAJOR_VERSION \
      -define:ERL_NIF_MINOR_VERSION=$ERL_NIF_MINOR_VERSION \
      -build-mode:shared -no-entry-point \
      -extra-linker-flags="-dynamiclib -undefined dynamic_lookup -fpic" \
      -out="$build_folder/${name}_nif.so"

    # Build Erlang Files
    erlc -o "$build_folder" $test_folder/*.erl
  }

  set_erlang_nif_version

  echo "Erlang NIF Version: $ERL_NIF_MAJOR_VERSION.$ERL_NIF_MINOR_VERSION"
  echo "Building tests: [Starting]"

  build_test "basic_load"
}

function run_integration_tests() {
  function run_test() {
    local name="$1"

    local build_folder="$TESTS_BASE/integration/$name/_build"

    echo "  - 'integration/$name'"

    cd "$build_folder"
    erl -noinput -eval "$name:run_tests()." -s init stop
  }

  echo "Running tests: [Starting]"

  run_test "basic_load"
}

# Call function by name
"$@"