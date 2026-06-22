let () =
  let opam_repo = Common.get_opam_repo () in
  let file = ref None in
  let cli_packages = ref [] in
  Arg.parse
    [ ( "-f"
      , Arg.String (fun s -> file := Some s)
      , "<file>  Read package list from file (one entry per line)" )
    ; ( "--file"
      , Arg.String (fun s -> file := Some s)
      , "<file>  Read package list from file (one entry per line)" )
    ; ("--debug", Arg.Set Common.debug, "  Enable debug output")
    ]
    (fun p -> cli_packages := p :: !cli_packages)
    "Usage: get_missing [--debug] [-f <file>] [package.version | package] ...";
  let file_packages =
    match !file with
    | None -> []
    | Some path -> Common.read_lines path
  in
  let args = List.rev !cli_packages @ file_packages in
  if args = [] then (
    print_endline
      "No packages specified. Provide package names as arguments or use -f <file>.";
    exit 1);
  Common.dlog "processing %d entr%s: %s" (List.length args)
    (if List.length args = 1 then "y" else "ies")
    (String.concat ", " args);
  let ns =
    Lwt_main.run
      (Lwt_list.map_p
         (fun arg ->
           match Common.package_of_string_opt arg with
           | Some p -> (
               Common.dlog "%s: explicit version" (OpamPackage.to_string p);
               try
                 let opam_p = Common.get_opam_file opam_repo p in
                 let open Lwt.Syntax in
                 let* r = Common.fetch_from_cache p opam_p in
                 Lwt.return (Option.to_list r)
               with e ->
                 print_endline
                   ("Error processing "
                    ^ OpamPackage.to_string p
                    ^ ": "
                    ^ Printexc.to_string e);
                 Lwt.return [])
           | None ->
               let name = OpamPackage.Name.of_string arg in
               let versions = Common.get_all_versions opam_repo name in
               if versions = [] then (
                 print_endline ("No versions found for package: " ^ arg);
                 Lwt.return [])
               else (
                 Common.dlog "%s: found %d version%s: %s" arg
                   (List.length versions)
                   (if List.length versions = 1 then "" else "s")
                   (String.concat ", "
                      (List.map OpamPackage.to_string versions));
                 Lwt_list.filter_map_p
                   (fun p ->
                     try
                       let opam_p = Common.get_opam_file opam_repo p in
                       Common.fetch_if_missing p opam_p
                     with e ->
                       print_endline
                         ("Error processing "
                          ^ OpamPackage.to_string p
                          ^ ": "
                          ^ Printexc.to_string e);
                       Lwt.return_none)
                   versions))
         args
       |> Lwt.map List.concat)
    |> List.map (fun (p, n) -> Common.make_saved_entry p n)
  in
  Common.save_to_saved_files ns
