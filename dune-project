(lang dune 3.0)
(name get_missing)
(generate_opam_files true)

(package
 (name get_missing)
 (allow_empty)
 (depends
	cohttp-lwt-unix
	clz
	lwt
 tls-lwt
	opam-file-format
	opam-state
	uri))
