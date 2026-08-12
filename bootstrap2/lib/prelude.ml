(* Declarations every program starts with, parsed and checked with it. Nothing
   here is reachable from the compiler: it is Cronyx, and a type declared in it
   is a type like any other.

   It becomes a file the module loader reads once there is one. *)

let source =
  {|
type List<T> {
    items: Array<T>,
    count: int
}

fn __list_of<T>(items: Array<T>) -> List<T> {
    return new List { items: items, count: items.len() };
}

fn __list_get<T>(self: List<T>, at: int) -> T {
    if (at < 0 or at >= self.count) {
        return self.items[self.count];
    }
    return self.items[at];
}

fn __list_set<T>(self: List<T>, at: int, v: T) -> T {
    if (at < 0 or at >= self.count) {
        return self.items[self.count];
    }
    self.items[at] = v;
    return v;
}

impl List<T> {
    fn len(self) -> int {
        return self.count;
    }

    fn push(self, v: T) {
        if (self.count == self.items.len()) {
            var room = self.count * 2;
            if (room < 4) {
                room = 4;
            }
            var bigger = new Array<T>(room, v);
            var i = 0;
            while (i < self.count) {
                bigger[i] = self.items[i];
                i = i + 1;
            }
            self.items = bigger;
        }
        self.items[self.count] = v;
        self.count = self.count + 1;
    }

    fn pop(self) -> T {
        self.count = self.count - 1;
        return self.items[self.count];
    }

    fn contains(self, v: T) -> bool {
        var i = 0;
        while (i < self.count) {
            if (self.items[i] == v) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }
}
|}

(* Parsed once and prepended to the program, so its declarations are in scope
   and go through every pass the program does. *)
let program () =
  match Scanner.scan_tokens source with
  | Error _ -> failwith "the prelude does not scan"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error (e :: _) ->
       failwith (Printf.sprintf "the prelude does not parse [%d:%d] %s" e.Parser.line e.Parser.col e.Parser.message)
     | Error [] -> failwith "the prelude does not parse"
     | Ok program -> program)

