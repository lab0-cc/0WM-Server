# 0WM Server

The 0WM Server is the central backend for 0WM. It receives Wi-Fi scans from the [0WM Client](https://github.com/lab0-cc/0WM-Client) and floorplan data from the [0WM OpMode](https://github.com/lab0-cc/0WM-OpMode), and stores and serves them, along with generated coverage heatmaps.

The 0WM project is in alpha stage. Interfaces and configuration may still change.

## Getting Started

### Debian packages

The easiest way to install the server is through your package manager. We provide nightly Debian packages for the following distributions and architectures:

| Distribution | Suites                  | Architectures |
| ------------ | ----------------------- | ------------- |
| Debian       | bookworm, trixie, forky | amd64, arm64  |
| Ubuntu       | jammy, noble            | amd64, arm64  |

### Nix development environment

A development environment can be entered with:

```bash
nix develop github:lab0-cc/0WM-Server -L
```

The project can be build with:

```bash
nix build github:lab0-cc/0WM-Server -L
```

### Building from sources

The server is written in OCaml. OPAM is the OCaml package manager, on which we rely in this project. Some of our dependencies are pinned from Git because required versions are not yet published on OPAM.

In the rest of this README, we refer to `0wm` and `0wmd` binaries; when building from sources using `dune`, those binaries are named `zwm` and `zwmd`. The latter names are used in this section only but apply to the whole document if your are building from sources.

#### Prerequisites

- OCaml (5+)
- Opam
- Git

#### Installation

```bash

# 1. Initialize OPAM if you haven’t already
opam init --compiler=ocaml-option-flambda --shell-setup
eval $(opam env)

# 2. Pin development dependencies (Irmin and Dream)
opam pin add --no-action --yes git+https://github.com/mirage/irmin
opam pin add --no-action --yes git+https://github.com/camlworks/dream

# 3. Clone and install dependencies
git clone https://github.com/lab0-cc/0WM-Server.git
cd 0WM-Server
opam install --confirm-level=unsafe-yes --deps-only .
```

#### Running the Server

To start the server daemon (which listens for API requests):

```bash
dune exec src/zwmd.exe
```

By default, it listens on port 8000.

In the rest of this document we use the `0wm` command. When using `dune`, it should be invoked with `dune exec src/zwm.exe -- <subcommand>` instead of `0wm <command>`.

## Configuration

The project ships with a CLI (`0wm`) to inspect/edit the daemon configuration:

```bash
0wm config show
```

To edit the settings:

```bash
0wm config edit
```

The most important keys are `interface` (bind address), `port`, `aps` (AP endpoints for clients to contact), and `ssids` (list of SSIDs to include in heatmaps).

### Typical development environment configuration

Use values similar to:

```yaml
interface: 127.0.0.1
port: 8000
aps:
  - http://127.0.0.1:8003
  - http://ap.local
ssids:
  - MySurveySSID
```

## API Documentation

An OpenAPI file is available in `static/api.yml`, and when the server is running, it is also available at `http://localhost:8000/api.yml`, with a Swagger interface at `http://localhost:8000/`.

## Troubleshooting

### Client connects but scanning/heatmap does not work

Confirm `aps` includes at least one reachable AP endpoint and `ssids` includes the SSIDs you want to visualize.

### Heatmaps look empty

Heatmap generation uses the `ssids` configuration key to filter SSIDs to use. If this list is empty, scans are still recorded but heatmaps will look empty/uniform.

## Funding

This project is funded through [NGI Zero Core](https://nlnet.nl/core), a fund established by [NLnet](https://nlnet.nl) with financial support from the European Commission's [Next Generation Internet](https://ngi.eu) program. Learn more at the [NLnet project page](https://nlnet.nl/project/0WM).

[<img src="https://nlnet.nl/logo/banner.png" alt="NLnet foundation logo" width="20%" />](https://nlnet.nl)
[<img src="https://nlnet.nl/image/logos/NGI0_tag.svg" alt="NGI Zero Logo" width="20%" />](https://nlnet.nl/core)
