type source_result = Unreachable | WrongChecksum | Verified of string

let check_url label url expected_hashes =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* content = Common.download_url url in
     match content with
     | None ->
         print_endline (label ^ ": not reachable: " ^ url);
         Lwt.return Unreachable
     | Some c ->
         if Common.verify_checksum c expected_hashes then (
           print_endline (label ^ ": OK (checksum verified): " ^ url);
           Lwt.return (Verified c))
         else (
           print_endline (label ^ ": wrong checksum: " ^ url);
           Lwt.return WrongChecksum))

let check_cache hash_paths expected_hashes =
  Lwt_main.run
    (let open Lwt.Syntax in
     let* content = Common.download_from_caches hash_paths in
     match content with
     | None ->
         print_endline "Cache: not reachable";
         Lwt.return Unreachable
     | Some c ->
         if Common.verify_checksum c expected_hashes then (
           print_endline "Cache: OK (checksum verified)";
           Lwt.return (Verified c))
         else (
           print_endline "Cache: wrong checksum";
           Lwt.return WrongChecksum))

let first_verified results =
  List.find_map (function Verified c -> Some c | _ -> None) results

let any_wrong_checksum results =
  List.exists (function WrongChecksum -> true | _ -> false) results

let () =
  let opam_repo = Common.get_opam_repo () in
  let positional = ref [] in
  Arg.parse
    [ ("--debug", Arg.Set Common.debug, "  Enable debug output") ]
    (fun s -> positional := s :: !positional)
    "Usage: check_source [--debug] <package.version> [<url>]";
  let package_str, url_opt =
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
  Common.dlog "expected checksums (%d):" (List.length expected_hashes);
  List.iter (fun h -> Common.dlog "  %s" h) expected_hashes;
  let hash_paths = Common.process opam_p in
  Common.dlog "cache paths: %s" (String.concat ", " hash_paths);
  let source_url = Common.get_source_url opam_p in
  (match source_url with
  | Some u -> Common.dlog "source url: %s" u
  | None -> Common.dlog "source url: none");
  let src_result =
    match source_url with
    | None ->
        print_endline "Source URL: none in opam file";
        Unreachable
    | Some url -> check_url "Source URL" url expected_hashes
  in
  let cli_result =
    Option.map (fun url -> check_url "CLI URL" url expected_hashes) url_opt
  in
  let cache_result = check_cache hash_paths expected_hashes in
  let all_results =
    (src_result :: Option.to_list cli_result) @ [ cache_result ]
  in
  let had_error = any_wrong_checksum all_results in
  match first_verified all_results with
  | None ->
      print_endline "No source with correct checksum found.";
      exit 1
  | Some _ when not had_error ->
      print_endline
        "All checked sources have correct checksums. No action needed."
  | Some content -> (
      print_endline
        "Checksum mismatch on at least one source; saving verified content.";
      try
        let pkg_str, filename = Common.save_package_file opam_p content in
        Common.save_to_saved_files [ Common.make_saved_entry pkg_str filename ];
        print_endline ("Saved " ^ filename ^ " and added to saved_files.txt")
      with e ->
        Printf.eprintf "Error saving file: %s\n" (Printexc.to_string e);
        exit 1)
