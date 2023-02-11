let () =
  let fname = try Sys.argv.(1) with _ -> "main.tl" in
  let file = open_in fname in
  let rec h s = try h (s ^ input_line file) with _ -> s in

  let s =
    h "" |> Typed_lisp.parse_code |> Typed_lisp.typecheck |> Typed_lisp.compile
  in
  let fout = open_out "main.ll" in
  let fout = Format.formatter_of_out_channel fout in
  Format.printf "%s\n" s;
  Format.fprintf fout "%s\n" s
