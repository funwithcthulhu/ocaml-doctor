module Check = Doctor.Check
module Editor = Doctor.Editor
module Opam = Doctor.Opam
module Platform = Doctor.Platform
module Process = Doctor.Process

let result ?(stdout = "") ?(stderr = "") status command args =
  { Process.command; args; status; stdout; stderr }

let run responses command args =
  match List.assoc_opt (command, args) responses with
  | Some (status, stdout, stderr) ->
      result ~stdout ~stderr status command args
  | None -> result (Process.Spawn_error "not found") command args

let expect_equal label expected actual =
  if not (String.equal expected actual) then
    failwith
      (Printf.sprintf "%s: expected %S, got %S" label expected actual)

let expect_int label expected actual =
  if expected <> actual then
    failwith
      (Printf.sprintf "%s: expected %d, got %d" label expected actual)

let expect_severity label expected actual =
  if expected <> actual then
    failwith (Printf.sprintf "%s: wrong severity" label)

let expect_some label = function
  | Some value -> value
  | None -> failwith (Printf.sprintf "%s: expected Some _" label)

let expect_suggestion label expected diagnostic =
  expect_equal label expected
    (expect_some (label ^ " suggestion") diagnostic.Check.suggestion)

let expect_detail label expected diagnostic =
  expect_equal label expected
    (expect_some (label ^ " detail") diagnostic.Check.detail)

let find_diagnostic id diagnostics =
  diagnostics
  |> List.find_opt (fun diagnostic ->
      String.equal diagnostic.Check.id id)
  |> expect_some ("diagnostic " ^ id)

let () =
  let command_responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("ocaml", [ "-version" ]),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      (("dune", [ "--version" ]), (Process.Exited 0, "3.17.0\n", ""));
      ( ("ocaml-lsp-server", [ "--version" ]),
        (Process.Spawn_error "not found", "", "") );
      (("ocamllsp", [ "--version" ]), (Process.Exited 0, "1.26.0\n", ""));
      ( ("ocamlformat", [ "--version" ]),
        (Process.Exited 0, "0.27.0\n", "") );
    ]
  in
  let diagnostics =
    Check.command_diagnostics ~run:(run command_responses)
  in
  let lsp = find_diagnostic "command.ocaml-lsp-server" diagnostics in
  expect_severity "lsp fallback" Check.Ok lsp.severity;
  expect_equal "lsp fallback title" "OCaml LSP found: 1.26.0 (ocamllsp)"
    lsp.title;
  let missing_ocamlformat =
    Check.command_diagnostic ~run:(run [])
      {
        command = "ocamlformat";
        args = [ "--version" ];
        label = "ocamlformat";
        missing_severity = Check.Warn;
        missing_suggestion = "opam install ocamlformat";
        version_parser = String.trim;
      }
  in
  expect_severity "missing command is warning" Check.Warn
    missing_ocamlformat.severity;
  expect_equal "missing command suggestion" "opam install ocamlformat"
    (expect_some "missing command suggestion"
       missing_ocamlformat.suggestion);
  let failing_opam =
    Check.command_diagnostic
      ~run:
        (run
           [
             ( ("opam", [ "--version" ]),
               (Process.Exited 2, "", "opam failed\n") );
           ])
      {
        command = "opam";
        args = [ "--version" ];
        label = "opam";
        missing_severity = Check.Error;
        missing_suggestion =
          "Install opam from https://opam.ocaml.org/doc/Install.html";
        version_parser = String.trim;
      }
  in
  expect_severity "nonzero command is diagnostic" Check.Error
    failing_opam.severity;

  let missing_tool_responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("ocaml", [ "-version" ]),
        (Process.Exited 0, "The OCaml toplevel, version 5.2.0\n", "") );
      ( ("dune", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
      ( ("ocaml-lsp-server", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
      ( ("ocamllsp", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
      ( ("ocamlformat", [ "--version" ]),
        (Process.Spawn_error "missing", "", "") );
    ]
  in
  let diagnostics =
    Check.command_diagnostics ~run:(run missing_tool_responses)
  in
  let dune = find_diagnostic "command.dune" diagnostics in
  expect_severity "missing dune is warning when opam exists" Check.Warn
    dune.severity;
  expect_equal "missing dune title" "dune not found" dune.title;
  expect_suggestion "missing dune suggestion" "opam install dune" dune;
  let lsp = find_diagnostic "command.ocaml-lsp-server" diagnostics in
  expect_severity "missing lsp is warning when opam exists" Check.Warn
    lsp.severity;
  expect_equal "missing lsp title" "OCaml LSP command not found"
    lsp.title;
  expect_detail "missing lsp detail"
    "Checked `ocaml-lsp-server` and `ocamllsp`; neither command is \
     available on PATH."
    lsp;
  expect_suggestion "missing lsp suggestion"
    "opam install ocaml-lsp-server" lsp;
  let ocamlformat = find_diagnostic "command.ocamlformat" diagnostics in
  expect_severity "missing ocamlformat is warning when opam exists"
    Check.Warn ocamlformat.severity;
  expect_equal "missing ocamlformat title" "ocamlformat not found"
    ocamlformat.title;
  expect_suggestion "missing ocamlformat suggestion"
    "opam install ocamlformat" ocamlformat;
  expect_int "missing optional tools exit code" 1
    (Check.exit_code diagnostics);

  let diagnostics = Opam.diagnostics ~run:(run []) Platform.Linux in
  let skipped = find_diagnostic "opam.initialized" diagnostics in
  expect_severity "missing opam skips opam checks as error" Check.Error
    skipped.severity;
  expect_equal "missing opam title"
    "opam checks skipped because opam is missing" skipped.title;
  expect_suggestion "missing opam suggestion"
    "Install opam from https://opam.ocaml.org/doc/Install.html" skipped;
  expect_int "missing opam exit code" 2 (Check.exit_code diagnostics);

  let opam_responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      ( ("opam", [ "var"; "root" ]),
        (Process.Exited 0, "/home/me/.opam\n", "") );
      (("opam", [ "switch"; "show" ]), (Process.Exited 0, "5.2.0\n", ""));
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n5.2.0\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, "/home/me/.opam/5.2.0/bin\n", "") );
      ( ("sh", [ "-c"; "command -v ocaml" ]),
        (Process.Exited 0, "/usr/bin/ocaml\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        (Process.Exited 0, "ocaml\ndune\nocaml-lsp-server\n", "") );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(run opam_responses) Platform.Linux
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "env sync warning" Check.Warn env.severity;
  expect_equal "env sync title"
    "shell environment may be out of sync with opam" env.title;
  expect_detail "env sync detail"
    "ocaml resolves to /usr/bin/ocaml, but the active switch bin is \
     /home/me/.opam/5.2.0/bin."
    env;
  expect_suggestion "env sync suggestion" "eval $(opam env)" env;
  let ocamlformat =
    find_diagnostic "opam.package.ocamlformat" diagnostics
  in
  expect_severity "missing ocamlformat package" Check.Warn
    ocamlformat.severity;

  let windows_responses =
    [
      (("opam", [ "--version" ]), (Process.Exited 0, "2.2.1\n", ""));
      (("opam", [ "var"; "root" ]), (Process.Exited 0, "C:\\opam\n", ""));
      ( ("opam", [ "switch"; "show" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "switch"; "list"; "--short" ]),
        (Process.Exited 0, "default\n", "") );
      ( ("opam", [ "var"; "bin" ]),
        (Process.Exited 0, "C:\\opam\\default\\bin\n", "") );
      ( ("where", [ "ocaml" ]),
        (Process.Exited 0, "C:\\OCaml\\bin\\ocaml.exe\n", "") );
      ( ("opam", [ "list"; "--installed"; "--short" ]),
        ( Process.Exited 0,
          "ocaml\ndune\nocaml-lsp-server\nocamlformat\n",
          "" ) );
    ]
  in
  let diagnostics =
    Opam.diagnostics ~run:(run windows_responses) Platform.Windows
  in
  let env = find_diagnostic "opam.env.sync" diagnostics in
  expect_severity "windows env sync warning" Check.Warn env.severity;
  expect_suggestion "windows env sync suggestion"
    "Run `opam env` and apply the environment changes in your current \
     shell, then restart the terminal if needed."
    env;

  let editor = Editor.diagnostics ~run:(run []) in
  let code = find_diagnostic "editor.vscode.command" editor in
  expect_severity "missing code is ok" Check.Ok code.severity
