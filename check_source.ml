let () =
  let opam_repo = Common.get_opam_repo () in
  let positional = ref [] in
  Arg.parse
    [ ("--debug", Arg.Set Common.debug, "  Enable debug output") ]
    (fun s -> positional := s :: !positional)
    "Usage: check_source [--debug] <package.version> [<url>]";
  let (package_str, url_opt) =
    match List.rev !positional with
    | [ p ] -> (p, None)
    | [ p; u ] -> (p, Some u)
    | [] ->
        print_endline
          "No package specified.\n\
           Usage: check_source [--debug] <package.version> [<url>]";
        exit 1
    | _ ->
        print_endline "Too many arguments.";
        exit 1
  in
  let p =
    match Common.package_of_string_opt package_str with
    | Some p -> p
    | None ->
        Printf.eprintf "Invalid package format: %s (expected name.version)\n"
          package_str;
        exit 1
  in
  let opam_p =
    try Common.get_opam_file opam_repo p
    with e ->
      Printf.eprintf "Error reading opam file for %s: %s\n" package_str
        (Printexc.to_string e);
      exit 1
  in
  let expected_hashes =
    try Common.checksums opam_p
    with e ->
      Printf.eprintf "Error getting checksums for %s: %s\n" package_str
        (Printexc.to_string e);
      exit 1
  in
  let hash_paths = Common.process opam_p in
  let cache_ok =
    Lwt_main.run
      (let open Lwt.Syntax in
       let* content = Common.download_from_caches hash_paths in
       match content with
       | None ->
           print_endline "Cache: not reachable";
           Lwt.return_none
       | Some c ->
           if Common.verify_checksum c expected_hashes then (
             print_endline "Cache: OK (checksum verified)";
             Lwt.return_some c)
           else (
             print_endline "Cache: wrong checksum";
             Lwt.return_none))
  in
  let url_ok =
    match url_opt with
    | None -> None
    | Some url ->
        Lwt_main.run
          (let open Lwt.Syntax in
           let* content = Common.download_url url in
           match content with
           | None ->
               print_endline ("URL: not reachable: " ^ url);
               Lwt.return_none
           | Some c ->
               if Common.verify_checksum c expected_hashes then (
                 print_endline ("URL: OK (checksum verified): " ^ url);
                 Lwt.return_some (c, url))
               else (
                 print_endline ("URL: wrong checksum: " ^ url);
                 Lwt.return_none))
  in
  match (cache_ok, url_ok) with
  | None, Some (content, _url) ->
      print_endline "Cache unavailable but URL is valid; saving file locally.";
      (try
         let pkg_str, filename = Common.save_package_file opam_p content in
         Common.save_to_saved_files [ Common.make_saved_entry pkg_str filename ];
         print_endline ("Saved " ^ filename ^ " and added to saved_files.txt")
       with e ->
         Printf.eprintf "Error saving file: %s\n" (Printexc.to_string e);
         exit 1)
  | Some _, _ ->
      print_endline
        "Cache source is accessible and checksum is valid. No action needed."
  | None, None ->
      print_endline
        "Both cache and URL (if provided) are unavailable or have wrong \
         checksum.";
      exit 1
