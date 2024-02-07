let caches = 
  [ "https://opam.ocaml.org/cache/"
  ; "https://gitlab.ocamlpro.com/OCamlPro/opam-repository/-/raw/cached/cache/"
  ; "https://opam.robur.coop/cache/"
  ]

let init_opam () =
  OpamSystem.init ();
  let root = OpamStateConfig.opamroot () in
  ignore (OpamStateConfig.load_defaults ~lock_kind:`Lock_read root);
  OpamFormatConfig.init ();
  OpamCoreConfig.init ~safe_mode:true ();
  OpamRepositoryConfig.init ();
  OpamStateConfig.init ~root_dir:root ()

let get_opam_repo () =
  let opam_root = OpamFilename.concat_and_resolve (OpamStateConfig.opamroot ()) "repo/default" in
  print_endline ("Using opam repository at: " ^ (OpamFilename.Dir.to_string opam_root));
  opam_root

let get_opam_file opam_repo p =
  let name = OpamPackage.Name.to_string p.OpamPackage.name in
  let opam_filename = OpamRepositoryPath.opam opam_repo (Some name) p in
  print_endline ("Reading from: " ^ (OpamFile.to_string opam_filename));
  OpamFile.OPAM.read opam_filename

let checksums opam_package =
  match OpamFile.OPAM.url opam_package with
  | None ->
    let name = OpamFile.OPAM.name opam_package |> OpamPackage.Name.to_string in 
    failwith ("Unable to find checksum for " ^ name)
  | Some url -> List.map (fun hash -> OpamHash.to_string hash) (OpamFile.URL.checksum url)

let prepare hash =
  match String.split_on_char '=' hash with
  | kind::hash::[] -> String.concat "/" [kind ; (String.sub hash 0 2) ; hash]
  | _ ->  failwith ("Malformed hash: " ^ hash)

let process opam_p =
  let checksums = checksums opam_p in
  List.map prepare checksums |> List.hd

let get_file cache url =
  let open Lwt.Syntax in
  let uri = Uri.of_string (cache ^ "/" ^ url) in
  let headers = Clz_cohttp.update_header None in
  let* res = Cohttp_lwt_unix.Client.get ~headers uri in
  let status = Cohttp.Response.status (fst res) |> Cohttp.Code.code_of_status in
  if status = 200 then
    let* body = Clz_cohttp.decompress res in
    Lwt.return_some body
  else Lwt.return_none

let rec retry url = function
    | [] -> Lwt.return_none
    | c :: rest -> let open Lwt.Syntax in
      let* body = get_file c url in
      if Option.is_some body then Lwt.return body else retry url rest 

let save_file opam_package content =
  let name = OpamFile.OPAM.name opam_package |> OpamPackage.Name.to_string in
  let version = OpamFile.OPAM.version opam_package |> OpamPackage.Version.to_string in
  let url = Option.map (OpamUrl.to_string) (OpamFile.OPAM.get_url opam_package) in
  let extension = match Option.map Filename.extension url with
    | Some ".gz" -> ".tar.gz"
    | Some ext -> ext 
    | None -> failwith "Unable to infer file extension from url"
  in
  let filename = name ^ "-" ^ version ^ extension in
  let oc = open_out_bin filename in
  Fun.protect
    (fun () -> output_string oc content)
    ~finally:(fun () -> close_out_noerr oc)

let () = 
  init_opam ();
  let opam_repo = get_opam_repo () in
  let p = OpamPackage.of_string "cohttp.5.1.0" in
  let opam_p = get_opam_file opam_repo p in
  let url = process opam_p in
  let content = Lwt_main.run @@ retry url caches in
  match content with
    | Some f -> save_file opam_p f
    | None -> print_endline ("Could not save " ^ (OpamPackage.to_string p))
