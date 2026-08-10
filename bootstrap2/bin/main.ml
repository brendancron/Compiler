open Bootstrap2

let source =
  {|fn fib(n) {
  if (n < 2) {
    return n;
  }
  return fib(n - 1) + fib(n - 2);
}

for (var k = 0; k < 10; k = k + 1) {
  print("fib(" + str(k) + ") =" , fib(k));
}

var total = 0;
var i = 0;
while (i < 5 and !false) {
  total = total + i;
  i = i + 1;
}
print("sum of 0..4 =", total);
|}

let report line col kind message =
  Printf.eprintf "[%d:%d] %s error: %s\n" line col kind message

let () =
  print_endline "-- program --";
  print_string source;
  match Scanner.scan_tokens source with
  | Error errors ->
    List.iter (fun (e : Scanner.error) -> report e.line e.col "Scan" e.message) errors;
    exit 65
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error errors ->
       List.iter (fun (e : Parser.error) -> report e.line e.col "Parse" e.message) errors;
       exit 65
     | Ok program ->
       print_endline "-- ast --";
       print_string (Printer.string_of_program program);
       print_endline "-- output --";
       (match Interp.run (Desugar.program program) with
        | Ok () -> ()
        | Error e ->
          report e.span.line e.span.col "Runtime" e.message;
          exit 70))
