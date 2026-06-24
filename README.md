# get_missing

A set of half-baked rushed-up utilities for archiving missing or unreachable OPAM packages still available in caches, and updating `ocaml/opam-repository` with the relevant fallback URLs.

The context for this is on the [opam-repository wiki](https://github.com/ocaml/opam-repository/wiki/FAQ#how-to-find-lost-archives-of-packages). In brief: when upstream URLs for packages in `opam-repository` become unreachable, this tool helps to automate fetching them from OCaml/Robur cache servers, and update the package definitions with the new archive URLs.

The archival in the [ocaml/opam-source-archives](https://github.com/ocaml/opam-source-archives) repository is manual and I don't have plans to automate it for the moment (even though it would benefit from some polishing).

Note that the commands have also a minimal help in case of doubts and a helpful (for me) debug flag. They were assembled for my own sake and automation, so the quality is what it is and I am not really intending to improve them more than I need.

## How to use

First install the dependencies with `opam pin get_missing`. Due to my laziness nothing really gets installed, besides the dependencies.

### 1. Download Missing/Unreachable Archives

Run `get_missing` to obtain the archive tarballs from OCaml cache servers, you pass the versions from a file or listing them in the form package-name or package-name.version.

In the first case it will get all versions of package-name which are currently unreachable but whose sources are still in the caches.

```bash
dune exec ./get_missing.exe -- package1 package2.version ...
```

The tool will download the archives in the `downloads/` folder and append a mapping of each package and its fallback URL to `saved_files.txt`.

### 2. Upload to GitHub

Manually upload all files in the `downloads/` directory to the [ocaml/opam-source-archives](https://github.com/ocaml/opam-source-archives) GitHub repository.

### 3. Update Local OPAM Repository

Run `update_opam` pointing to your local clone of the [ocaml/opam-repository](https://github.com/ocaml/opam-repository), possibly checked out to a new branch.

```bash
dune exec ./update_opam.exe -- /path/to/local/opam-repository
```

This reads the entries from `saved_files.txt`, resolves the corresponding `opam` files in the local repository, and updates their `url` block with the new `opam-source-archives` URLs.

### 4. Open a Pull Request

From your local copy of `ocaml/opam-repository`, commit the changes, push the branch to your repo and submit a Pull Request to upstream `ocaml/opam-repository`.

---

## Use of LLMs

Most of the code is written by me, except `get_source.ml` and a PR by Claude, and the deduplication of urls by `gemma4-e4b` run locally but then edited by hand (with a small refactor by Claude and a hand polish). None of this code has been accepted without me checking it first.
