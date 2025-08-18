# 0WM Server

This repository contains the sources for the 0WM server. The 0WM project is far from being production-grade yet, but some of its components are already testable.

## Getting 0WM running

Currently, 0WM is not packaged and some of its dependencies are not live on OPAM. Therefore, a few manual operations are needed. We give here the required commands to get started on a fresh Debian Trixie container:

```bash
# Install git and opam
apt install --assume-yes git opam
# Init OPAM
opam init --compiler=5.2.1 --shell-setup
# Update the environment
eval $(opam env)
# Pin the unreleased Gendarme dependencies
opam pin add --no-action --yes git+https://github.com/bensmrs/gendarme
# Clone this repository
git clone https://github.com/lab0-cc/0WM-Server.git
# Enter the newly created directory
cd 0WM-Server
# Install the dependencies
opam install --confirm-level=unsafe-yes --deps-only .
# Compile and run the server
dune exec src/zwm.exe
```

By default, the server listens to `127.0.0.1:8000`; this can be configured in the `config.json` file.