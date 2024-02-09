let get_opam_repo () =
  OpamFilename.Dir.of_string "/Users/mseri/code/opam-repository"

let opam_filename opam_repo p =
  let name = OpamPackage.Name.to_string p.OpamPackage.name in
  OpamRepositoryPath.opam opam_repo (Some name) p

let get_opam_file opam_filename =
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

let () = 
  let opam_repo = get_opam_repo () in
  (* "filelist.txt" is a list name.version *)
  let ps = read "saved_files.txt"
    |> List.filter_map (fun s ->
      match String.split_on_char ' ' s with
      | [pkg; url] ->
        Some (opam_filename opam_repo @@ OpamPackage.of_string pkg, url)
      | _ -> None)
  in
  List.iter (fun (fn, url) ->
    let opam_p = get_opam_file fn in
    let checksum = match OpamFile.OPAM.url opam_p with
      | Some url -> OpamFile.URL.checksum url 
      | None -> []
    in 
    let url_p = OpamFile.URL.create ~checksum (OpamUrl.of_string url) in
    let opam_p = OpamFile.OPAM.with_url url_p opam_p in
    OpamFile.OPAM.write fn opam_p) ps

