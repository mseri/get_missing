let caches =
  [ "https://opam.ocaml.org/cache/"; "https://opam.robur.coop/cache/" ]

let get_opam_repo () =
  let opam_root =
    OpamFilename.concat_and_resolve (OpamStateConfig.opamroot ()) "repo/default"
  in
  print_endline
    ("Using opam repository at: " ^ OpamFilename.Dir.to_string opam_root);
  opam_root

let get_opam_file opam_repo p =
  let name = OpamPackage.Name.to_string p.OpamPackage.name in
  let opam_filename = OpamRepositoryPath.opam opam_repo (Some name) p in
  print_endline ("Reading from: " ^ OpamFile.to_string opam_filename);
  OpamFile.OPAM.read opam_filename

let package_of_string_opt s =
  try Some (OpamPackage.of_string s) with _ -> None

let get_all_versions opam_repo name =
  let name_str = OpamPackage.Name.to_string name in
  let pkg_dir =
    OpamFilename.Op.(OpamRepositoryPath.packages_dir opam_repo / name_str)
  in
  if not (OpamFilename.exists_dir pkg_dir) then []
  else
    OpamFilename.dirs pkg_dir
    |> List.filter_map (fun dir ->
           let base = Filename.basename (OpamFilename.Dir.to_string dir) in
           match package_of_string_opt base with
           | Some pkg when OpamPackage.name pkg = name -> Some pkg
           | _ -> None)

let checksums opam_package =
  match OpamFile.OPAM.url opam_package with
  | None ->
      let name =
        OpamFile.OPAM.name opam_package |> OpamPackage.Name.to_string
      in
      failwith ("Unable to find checksum for " ^ name)
  | Some url ->
      List.map (fun hash -> OpamHash.to_string hash) (OpamFile.URL.checksum url)

let prepare hash =
  match String.split_on_char '=' hash with
  | [ kind; hash ] -> String.concat "/" [ kind; String.sub hash 0 2; hash ]
  | _ -> failwith ("Malformed hash: " ^ hash)

let process opam_p =
  let checksums = checksums opam_p in
  List.map prepare checksums

let get_file cache url =
  let open Lwt.Syntax in
  let uri = Uri.of_string (cache ^ "/" ^ url) in
  let headers = Clz_cohttp.update_header None in
  let* res = Cohttp_lwt_unix.Client.get ~headers uri in
  let status = Cohttp.Response.status (fst res) |> Cohttp.Code.code_of_status in
  if status = 200 || status = 302 then
    let* body = Clz_cohttp.decompress res in
    Lwt.return_some body
  else Lwt.return_none

let rec retry url = function
  | [] -> Lwt.return_none
  | c :: rest ->
      let open Lwt.Syntax in
      let* body = get_file c url in
      if Option.is_some body then Lwt.return body else retry url rest

let source_url_reachable url =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      let uri = Uri.of_string url in
      let headers = Clz_cohttp.update_header None in
      let* resp = Cohttp_lwt_unix.Client.head ~headers uri in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      Lwt.return (status >= 200 && status < 400))
    (fun _ -> Lwt.return false)

let write content filename =
  Out_channel.with_open_bin filename (fun oc ->
      Out_channel.output_string oc content)

let[@tail_mod_cons] rec input_trimmed_lines ic =
  match input_line ic with
  | line -> String.trim line :: input_trimmed_lines ic
  | exception End_of_file -> []

let read_lines filename =
  In_channel.with_open_text filename input_trimmed_lines
  |> List.filter (fun s -> s <> "")

let save_package_file opam_package content =
  let name = OpamFile.OPAM.name opam_package |> OpamPackage.Name.to_string in
  let version =
    OpamFile.OPAM.version opam_package |> OpamPackage.Version.to_string
  in
  let url = Option.map OpamUrl.to_string (OpamFile.OPAM.get_url opam_package) in
  let extension =
    match Option.map Filename.extension url with
    | Some ".gz" -> ".tar.gz"
    | Some ext -> ext
    | None -> failwith "Unable to infer file extension from url"
  in
  let filename = name ^ "-" ^ version ^ extension in
  write content filename;
  (name ^ "." ^ version, filename)

let rec get p opam_p urls =
  match urls with
  | [] -> Lwt.return_none
  | url :: rest -> (
      let open Lwt.Syntax in
      let* content = retry url caches in
      match content with
      | Some f -> (
          try Lwt.return_some (save_package_file opam_p f)
          with e ->
            print_endline (Printexc.to_string e);
            Lwt.return_none)
      | None ->
          print_endline
            ("Could not save " ^ OpamPackage.to_string p ^ " from " ^ url);
          get p opam_p rest)

let is_available opam_p =
  OpamFilter.to_string (OpamFile.OPAM.available opam_p) <> "false"

let fetch_from_cache p opam_p =
  if not (is_available opam_p) then Lwt.return_none
  else (
    try
      let urls = process opam_p in
      get p opam_p urls
    with _ -> Lwt.return_none)

let fetch_if_missing p opam_p =
  let open Lwt.Syntax in
  if not (is_available opam_p) then Lwt.return_none
  else
    match OpamFile.OPAM.url opam_p with
    | None -> Lwt.return_none
    | Some opam_url ->
        let src_url = OpamUrl.to_string (OpamFile.URL.url opam_url) in
        let* reachable = source_url_reachable src_url in
        if reachable then Lwt.return_none
        else (
          print_endline
            ("Source URL unreachable for "
             ^ OpamPackage.to_string p
             ^ ", fetching from cache");
          let urls = process opam_p in
          get p opam_p urls)

let () =
  let opam_repo = get_opam_repo () in
  let file = ref None in
  let cli_packages = ref [] in
  Arg.parse
    [ ( "-f"
      , Arg.String (fun s -> file := Some s)
      , "<file>  Read package list from file (one entry per line)" )
    ; ( "--file"
      , Arg.String (fun s -> file := Some s)
      , "<file>  Read package list from file (one entry per line)" )
    ]
    (fun p -> cli_packages := p :: !cli_packages)
    "Usage: get_missing [-f <file>] [package.version | package] ...";
  let file_packages =
    match !file with
    | None -> []
    | Some path -> read_lines path
  in
  let args = List.rev !cli_packages @ file_packages in
  if args = [] then (
    print_endline
      "No packages specified. Provide package names as arguments or use -f <file>.";
    exit 1);
  let ns =
    Lwt_main.run
      (Lwt_list.map_p
         (fun arg ->
           match package_of_string_opt arg with
           | Some p -> (
               try
                 let opam_p = get_opam_file opam_repo p in
                 let open Lwt.Syntax in
                 let* r = fetch_from_cache p opam_p in
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
               let versions = get_all_versions opam_repo name in
               if versions = [] then (
                 print_endline ("No versions found for package: " ^ arg);
                 Lwt.return [])
               else
                 Lwt_list.filter_map_p
                   (fun p ->
                     try
                       let opam_p = get_opam_file opam_repo p in
                       fetch_if_missing p opam_p
                     with e ->
                       print_endline
                         ("Error processing "
                          ^ OpamPackage.to_string p
                          ^ ": "
                          ^ Printexc.to_string e);
                       Lwt.return_none)
                   versions)
         args
       |> Lwt.map List.concat)
    |> List.map (fun (p, n) ->
           p ^ " https://github.com/ocaml/opam-source-archives/raw/main/" ^ n)
  in
  write (String.concat "\n" ns) "saved_files.txt"
