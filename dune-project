(lang dune 3.16)

(name smashtb)

(generate_opam_files true)

(source (github smelc/smashtb))
(authors "Clement Hurlin")
(maintainers "smelc@users.noreply.github.com")
(license MIT)

(package
 (name smashtb)
 (allow_empty)
 (synopsis "Single-page app to review, approve and dismiss GitHub pull requests")
 (description
  "smashtb takes a list of GitHub pull request links and shows, for each one, the files it modifies, a diff view of every file, and buttons to approve or dismiss the pull request.")
 (depends
  (ocaml (>= 5.5.0))
  (dune (>= 3.16))
  (brr (>= 0.0.6))
  (js_of_ocaml-compiler (>= 5.8))))
