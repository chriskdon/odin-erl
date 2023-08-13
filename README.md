<a name="readme-top"></a>
# odin-erl

*Version:* `v0.0.0-alpha` (DO NOT USE)

<!-- PROJECT SHIELDS -->

[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- TABLE OF CONTENTS -->
<details>
  <summary>Table of Contents</summary>
  <ol>
    <li>
      <a href="#about-the-project">About The Project</a>
      <ul>
        <li><a href="#built-with">Built With</a></li>
      </ul>
    </li>
    <li>
      <a href="#getting-started">Getting Started</a>
    </li>
    <li><a href="#usage">Usage</a></li>
    <li><a href="#project-structure">Project Structure</a></li>
    <li><a href="#todo">TODO</a></li>
    <li><a href="#contributing">Contributing</a></li>
    <li><a href="#license">License</a></li>
  </ol>
</details>

<!-- ABOUT THE PROJECT -->
## About the Project

Odin bindings to native Erlang libraries: `erl_nif`, `erl_driver`, `erl_interface`.

> :warning: **None** of this has been tested (yet). I initially created this project
> as a way to learn Odin, and get a deeper understanding of Erlang NIFs.

### Built With

<!-- Tools the project is built with -->

[![Odin][Odin-badge]][Odin-url]
[![Erlang][Erlang-badge]][Erlang-url]
[![ASDF][ASDF-badge]][ASDF-url]

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- GETTING STARTED -->
## Getting Started

1. Make sure [ASDF](https://asdf-vm.com/) is installed
2. Install the Odin ASDF plugin: `asdf plugin add odin`
3. Install the Erlang ASDF plugin: `asdf plugin add erlang`
4. Install ASDF tools: `asdf install`
5. `make run_tests`
    - The tests don't do much at the moment other than load and call a
      simple NIF written in Odin.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- USAGE EXAMPLES -->
## Usage

1. Create a simple NIF in Odin

    ```odin
    // example_nif.odin

    package example_nif

    import "core:fmt"
    import "core:c"
    import "core:runtime"

    // Change to the path of wherever you've put the package
    import nif "../odin_erl_nif"
    import enif "../odin_erl_nif/erl_nif"

    // Name of the Erlang module the NIFs are being loaded into
    ERLANG_MODULE_NAME :: "example_nif"

    @export
    odin_add : enif.ErlangFFIPointer :  proc "c" (env: ^enif.ErlNifEnv, argc: c.int, argv: [^]enif.ERL_NIF_TERM) -> enif.ERL_NIF_TERM {
      x: c.int
      if (enif.get_int(env, argv[0], &x) == 0) {
        return enif.make_badarg(env);
      }

      y: c.int
      if (enif.get_int(env, argv[1], &y) == 0) {
        return enif.make_badarg(env);
      }

      return enif.make_int(env, x + y)
    }

    // List of functions to export as NIFs
    // {<erlang function name>, <arity>, <function pointer>, <flags>}
    nif_funcs := [?]enif.ErlNifFunc {
      {"odin_add", 2, odin_add, 0},
    }

    // Register the NIFs with Erlang
    @(init, private)
    _ :: proc() {
      nif.init(ERLANG_MODULE_NAME, nif_funcs[:])
    }
    ```

2. Create an Erlang file to load the nif

    ```erlang
    % example_nif.erl

    -module(example_nif).

    -export([odin_add/2]).

    -nifs([odin_add/2]).

    -on_load(init/0).

    init() ->
      ok = erlang:load_nif("./example_nif", 0).

    odin_add(_X, _Y) ->
      exit(nif_library_not_loaded).
    ```

3. Compile Odin Library

    Get your Erlang major and minor version with: `erlang:system_info(nif_version).`

    ```bash
    odin build "<project folder>" \
      -define:ERL_NIF_MAJOR_VERSION=<nif_version major version> \
      -define:ERL_NIF_MINOR_VERSION=<nif_version minor version> \
      -build-mode:shared -no-entry-point \
      -extra-linker-flags="-dynamiclib -undefined dynamic_lookup -fpic" \
      -out="example_nif.so"
    ```

4. Start an erlang shell with `erl`

    ```
    1> c(example_nif).
    2> example_nif:odin_add(2, 3).
    5
    ```

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- PROJECT STRUCTURE -->

## Project Structure

- `scripts`
  - Helper scripts to build the project.
- `src`
  - `erl_nif`
    - Low level bindings for `erl_nif`, and `erL_driver`. The functions and
      types in this packages should very closely follow the structure and
      format of the native libraries.
  - `odin_erl_nif`
    - High level helpers that make the low level `erl_nif` bindings nicer to
      work with.
- `tests`
  - Test directory


<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ROADMAP/TODO -->

## TODO

- [X] Create bindings for functions and types in `erl_nif`
  - [X] Functions
  - [X] Types
- [~] Create bindings for functions and types in `erl_driver`
  - [ ] Functions
  - [~] Types
- [ ] Create bindings for functions and types in `erl_interface`
  - [ ] Functions
  - [ ] Types
- [ ] Setup test framework
- [ ] Write tests for all `erl_nif` functions
- [ ] Write tests for all `erl_driver` functions
- [ ] Write tests for all `erl_interface` functions
- [ ] Create Odin allocator that uses the Erlang allocator
- [ ] Add support for rebar3
- [ ] Create helper library that wraps the low level functions with a higher-level
      interface.

See the [open issues](https://github.com/chriskdon/odin-erl/issues)
for a full list of proposed features (and known issues).

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTRIBUTING -->
## Contributing

Contributions are what make the open source community such an amazing place to
learn, inspire, and create. Any contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- LICENSE -->
## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- MARKDOWN LINKS & IMAGES
  Useful Links
  - https://www.markdownguide.org/basic-syntax/#reference-style-links
  - https://shields.io/
  - https://simpleicons.org/
-->

<!-- Generic Links -->
[contributors-shield]: https://img.shields.io/github/contributors/chriskdon/odin-erl.svg?style=for-the-badge
[contributors-url]: https://github.com/chriskdon/odin-erl/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/chriskdon/odin-erl.svg?style=for-the-badge
[forks-url]: https://github.com/chriskdon/odin-erl/network/members
[stars-shield]: https://img.shields.io/github/stars/chriskdon/odin-erl.svg?style=for-the-badge
[stars-url]: https://github.com/chriskdon/odin-erl/stargazers
[issues-shield]: https://img.shields.io/github/issues/chriskdon/odin-erl.svg?style=for-the-badge
[issues-url]: https://github.com/chriskdon/odin-erl/issues
[license-shield]: https://img.shields.io/github/license/chriskdon/odin-erl.svg?style=for-the-badge
[license-url]: https://github.com/chriskdon/odin-erl/blob/main/LICENSE.txt

<!-- Built With Links (see: https://shields.io/badges) -->
[ASDF-badge]: https://img.shields.io/badge/ASDF-000000?style=for-the-badge&logoColor=white
[ASDF-url]: https://asdf-vm.com/

[Odin-badge]: https://img.shields.io/badge/Odin-000000?style=for-the-badge&logoColor=white
[Odin-url]: https://odin-lang.org/

[Erlang-badge]: https://img.shields.io/badge/Erlang-000000?style=for-the-badge&logo=erlang&logoColor=white
[Erlang-url]: https://www.erlang.org/
