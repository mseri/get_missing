let get_opam_repo () =
  OpamFilename.Dir.of_string "/Users/mseri/code/opam-repository"

let get_opam_file opam_repo p =
  let name = OpamPackage.Name.to_string p.OpamPackage.name in
  let opam_filename = OpamRepositoryPath.opam opam_repo (Some name) p in
  print_endline ("Reading from: " ^ (OpamFile.to_string opam_filename));
  OpamFile.OPAM.read opam_filename

let write content filename =
  Out_channel.with_open_bin filename
    (fun oc -> Out_channel.output_string oc content)

let [@tail_mod_cons] rec input_trimmed_lines ic =
  match input_line ic with
  | line -> String.trim line :: input_trimmed_lines ic
  | exception End_of_file -> []

let read filename =
  In_channel.with_open_text filename input_trimmed_lines

let _save_package_file opam_package content =
  let name = OpamFile.OPAM.name opam_package |> OpamPackage.Name.to_string in
  let version = OpamFile.OPAM.version opam_package |> OpamPackage.Version.to_string in
  let url = Option.map (OpamUrl.to_string) (OpamFile.OPAM.get_url opam_package) in
  let extension = match Option.map Filename.extension url with
    | Some ".gz" -> ".tar.gz"
    | Some ext -> ext 
    | None -> failwith "Unable to infer file extension from url"
  in
  let filename = name ^ "-" ^ version ^ extension in
  write content filename;
  name ^ "." ^ version, filename

let () = 
  let opam_repo = get_opam_repo () in
  (* "filelist.txt" is a list name.version *)
  let ps = read "saved_files.txt"
    |> List.filter_map (fun s ->
      match String.split_on_char ' ' s with
      | [pkg; url] -> Some (get_opam_file opam_repo @@ OpamPackage.of_string pkg, url)
      | _ -> None)
  in
  List.iter (fun (_, u) -> print_endline u) ps
  (*
  let ns = Lwt_list.filter_map_p (fun p ->
    let open Lwt.Syntax in
    let opam_p = get_opam_file opam_repo p in
    if OpamFilter.to_string (OpamFile.OPAM.available opam_p) = "false" then Lwt.return_none
    else begin
      let url = process opam_p in
      let* content = retry url caches in
      match content with
      | Some f -> begin
        try
          Lwt.return_some (save_package_file opam_p f)
        with e -> 
          print_endline (Printexc.to_string e);
          Lwt.return_none
        end
      | None -> print_endline ("Could not save " ^ (OpamPackage.to_string p)); Lwt.return_none
      end) ps
  in
  let ns = Lwt_main.run ns |> List.map (fun (p ,n) -> p ^ " https://github.com/ocaml/opam-source-archives/raw/main/" ^ n) in
  write (String.concat "\n" ns) "saved_files.txt"
*)
