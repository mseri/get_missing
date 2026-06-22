let caches =
  [ "https://opam.ocaml.org/cache/"; "https://opam.robur.coop/cache/" ]

let debug = ref false
let dlog fmt = Printf.ksprintf (fun s -> if !debug then print_endline ("[debug] " ^ s)) fmt

let get_opam_repo () =
  let opam_root =
    OpamFilename.concat_and_resolve (OpamStateConfig.opamroot ()) "repo/default"
  in
  print_endline ("Using opam repository at: " ^ OpamFilename.Dir.to_string opam_root);
  opam_root

let get_opam_file opam_repo p =
  let name = OpamPackage.Name.to_string p.OpamPackage.name in
  let opam_filename = OpamRepositoryPath.opam opam_repo (Some name) p in
  dlog "reading opam file: %s" (OpamFile.to_string opam_filename);
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
  dlog "GET %s/%s" cache url;
  let headers = Clz_cohttp.update_header None in
  let* res = Cohttp_lwt_unix.Client.get ~headers uri in
  let status = Cohttp.Response.status (fst res) |> Cohttp.Code.code_of_status in
  dlog "  -> HTTP %d" status;
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

let rec download_from_caches = function
  | [] -> Lwt.return_none
  | url :: rest ->
      let open Lwt.Syntax in
      let* content = retry url caches in
      (match content with
      | Some _ -> Lwt.return content
      | None -> download_from_caches rest)

let download_url url =
  let open Lwt.Syntax in
  let uri = Uri.of_string url in
  dlog "GET %s" url;
  let headers = Clz_cohttp.update_header None in
  let* res = Cohttp_lwt_unix.Client.get ~headers uri in
  let status = Cohttp.Response.status (fst res) |> Cohttp.Code.code_of_status in
  dlog "  -> HTTP %d" status;
  if status = 200 || status = 302 then
    let* body = Clz_cohttp.decompress res in
    Lwt.return_some body
  else Lwt.return_none

let source_url_reachable url =
  let open Lwt.Syntax in
  Lwt.catch
    (fun () ->
      dlog "HEAD %s" url;
      let uri = Uri.of_string url in
      let headers = Clz_cohttp.update_header None in
      let* resp = Cohttp_lwt_unix.Client.head ~headers uri in
      let status = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
      let reachable = status >= 200 && status < 400 in
      dlog "  -> HTTP %d (%s)" status
        (if reachable then "reachable" else "unreachable");
      Lwt.return reachable)
    (fun e ->
      dlog "  -> error: %s" (Printexc.to_string e);
      Lwt.return false)

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

let downloads_dir = "downloads"

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
  (match Sys.is_directory downloads_dir with
  | exception Sys_error _ -> Sys.mkdir downloads_dir 0o755
  | false -> failwith (downloads_dir ^ " exists but is not a directory")
  | true -> ());
  let path = Filename.concat downloads_dir filename in
  write content path;
  dlog "saved %s.%s as %s" name version path;
  (name ^ "." ^ version, filename)

let github_archive_base =
  "https://github.com/ocaml/opam-source-archives/raw/main/"

let make_saved_entry pkg_str filename = pkg_str ^ " " ^ github_archive_base ^ filename

let save_to_saved_files entries =
  match entries with
  | [] -> ()
  | _ ->
      let exists = Sys.file_exists "saved_files.txt" in
      if exists then
        print_endline
          "Warning: saved_files.txt already exists; appending new entries.";
      let oc =
        Out_channel.open_gen
          (if exists then [ Open_append; Open_text ]
           else [ Open_wronly; Open_creat; Open_trunc; Open_text ])
          0o644 "saved_files.txt"
      in
      Fun.protect
        ~finally:(fun () -> Out_channel.close oc)
        (fun () ->
          if exists then Out_channel.output_char oc '\n';
          Out_channel.output_string oc (String.concat "\n" entries))

let verify_checksum content hash_strings =
  let tmp = Filename.temp_file "check_source" "tmp" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove tmp with _ -> ())
    (fun () ->
      write content tmp;
      List.exists
        (fun hash_str ->
          try
            let expected = OpamHash.of_string hash_str in
            let kind = OpamHash.kind expected in
            let computed = OpamHash.compute ~kind tmp in
            OpamHash.to_string expected = OpamHash.to_string computed
          with _ -> false)
        hash_strings)

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
  if not (is_available opam_p) then (
    dlog "%s: marked unavailable, skipping" (OpamPackage.to_string p);
    Lwt.return_none)
  else (
    dlog "%s: fetching from cache (explicit version)" (OpamPackage.to_string p);
    try
      let urls = process opam_p in
      get p opam_p urls
    with _ -> Lwt.return_none)

let fetch_if_missing p opam_p =
  let open Lwt.Syntax in
  if not (is_available opam_p) then (
    dlog "%s: marked unavailable, skipping" (OpamPackage.to_string p);
    Lwt.return_none)
  else
    match OpamFile.OPAM.url opam_p with
    | None ->
        dlog "%s: no source URL, skipping" (OpamPackage.to_string p);
        Lwt.return_none
    | Some opam_url ->
        let src_url = OpamUrl.to_string (OpamFile.URL.url opam_url) in
        let* reachable = source_url_reachable src_url in
        if reachable then (
          dlog "%s: source reachable, skipping" (OpamPackage.to_string p);
          Lwt.return_none)
        else (
          print_endline
            ("Source URL unreachable for "
             ^ OpamPackage.to_string p
             ^ ", fetching from cache");
          let urls = process opam_p in
          get p opam_p urls)
