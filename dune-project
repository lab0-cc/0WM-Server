(lang dune 2.7)
(version 0.1.0)
(generate_opam_files true)
(license MIT)
(authors "Benjamin Somers <benjamin.somers@resel.fr>")
(maintainers "Benjamin Somers <benjamin.somers@resel.fr>")
(homepage "https://github.com/lab0-cc/0WM-Server.git")
(source (github "lab0-cc/0WM-Server"))
(bug_reports "https://github.com/lab0-cc/0WM-Server/issues")

(package
 (name 0wm-server)
 (synopsis "0WM Server")
 (depends
  base64
  camlimages
  domainslib
  dream
  gendarme-csv
  gendarme-ezjsonm
  gendarme-toml
  gendarme-yaml
  irmin-git
  lwt_ppx
  ppx_marshal
  uuidm))
