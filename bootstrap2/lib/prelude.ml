(* Declarations every program starts with, parsed and checked with it. Nothing
   here is reachable from the compiler, save for the three types `typeof`
   answers with: it is Cronyx, and a type declared in it is a type like any
   other.

   It becomes a file the module loader reads once there is one. *)

let source =
  {|
// What a type says about itself. Reflect builds these, so their shape is
// fixed: renaming a field here changes what the compiler emits. The names are
// long because the prelude is one namespace and a program is free to declare
// its own Shape; they belong to a reflect module once the prelude is a file.
type TypeField { name: Name }
type TypeVariant { name: Name, arity: int }
type TypeShape {
    Scalar,
    Product(Name, Array<TypeField>),
    Sum(Name, Array<TypeVariant>),
    Other,
}

impl Array<T> {
    fn contains(self, v: T) -> bool {
        var i = 0;
        while (i < self.len()) {
            if (self[i] == v) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }
}

// Nothing in Cronyx can raise, so an index a list must reject is aimed past its
// backing array and the array's own bounds check does the rejecting. Passing the
// index straight through would not do: the spare capacity behind `count` holds
// values that were left there.
fn __past_end<T>(items: Array<T>) -> int {
    return items.len();
}

type List<T> {
    items: Array<T>,
    count: int
}

op []<T>(items: Array<T>) -> List<T> {
    return new List { items: items, count: items.len() };
}

op []<T>(self: List<T>, at: int) -> T {
    if (at < 0 || at >= self.count) {
        return self.items[__past_end(self.items)];
    }
    return self.items[at];
}

op []=<T>(self: List<T>, at: int, v: T) -> T {
    if (at < 0 || at >= self.count) {
        return self.items[__past_end(self.items)];
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

type Set<T> {
    items: List<T>
}

impl Set<T> {
    fn len(self) -> int {
        return self.items.len();
    }

    fn contains(self, v: T) -> bool {
        return self.items.contains(v);
    }

    fn insert(self, v: T) {
        if (!self.items.contains(v)) {
            self.items.push(v);
        }
    }
}

op []<T>(items: Array<T>) -> Set<T> {
    var out = new Set { items: [] };
    var i = 0;
    while (i < items.len()) {
        out.insert(items[i]);
        i = i + 1;
    }
    return out;
}

type Map<K, V> {
    entries: List<(K, V)>
}

impl Map<K, V> {
    fn len(self) -> int {
        return self.entries.len();
    }

    fn contains(self, key: K) -> bool {
        var i = 0;
        while (i < self.entries.len()) {
            if (self.entries[i].0 == key) {
                return true;
            }
            i = i + 1;
        }
        return false;
    }

    fn get_or(self, key: K, fallback: V) -> V {
        var i = 0;
        while (i < self.entries.len()) {
            if (self.entries[i].0 == key) {
                return self.entries[i].1;
            }
            i = i + 1;
        }
        return fallback;
    }

    fn insert(self, key: K, value: V) {
        var i = 0;
        while (i < self.entries.len()) {
            if (self.entries[i].0 == key) {
                self.entries[i] = (key, value);
                return;
            }
            i = i + 1;
        }
        self.entries.push((key, value));
    }
}

op []<K, V>(pairs: Array<(K, V)>) -> Map<K, V> {
    var out = new Map { entries: [] };
    var i = 0;
    while (i < pairs.len()) {
        out.insert(pairs[i].0, pairs[i].1);
        i = i + 1;
    }
    return out;
}

fn __is_space(c: char) -> bool {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

fn __slice(text: string, start: int, stop: int) -> string {
    var out = "";
    var i = start;
    while (i < stop) {
        out = out + str(text[i]);
        i = i + 1;
    }
    return out;
}

impl string {
    fn chars(self) -> Array<char> {
        var out = new Array<char>(self.len(), ' ');
        var i = 0;
        while (i < self.len()) {
            out[i] = self[i];
            i = i + 1;
        }
        return out;
    }

    fn contains(self, needle: string) -> bool {
        var start = 0;
        while (start + needle.len() <= self.len()) {
            var i = 0;
            var same = true;
            while (same && i < needle.len()) {
                if (self[start + i] != needle[i]) {
                    same = false;
                }
                i = i + 1;
            }
            if (same) {
                return true;
            }
            start = start + 1;
        }
        return false;
    }

    fn split(self, sep: char) -> Array<string> {
        var parts: List<string> = [];
        var start = 0;
        var i = 0;
        while (i < self.len()) {
            if (self[i] == sep) {
                parts.push(__slice(self, start, i));
                start = i + 1;
            }
            i = i + 1;
        }
        parts.push(__slice(self, start, self.len()));

        var out = new Array<string>(parts.len(), "");
        var j = 0;
        while (j < parts.len()) {
            out[j] = parts[j];
            j = j + 1;
        }
        return out;
    }

    fn trim(self) -> string {
        var start = 0;
        while (start < self.len() && __is_space(self[start])) {
            start = start + 1;
        }
        var stop = self.len();
        while (stop > start && __is_space(self[stop - 1])) {
            stop = stop - 1;
        }
        return __slice(self, start, stop);
    }
    fn starts_with(self, prefix: string) -> bool {
        if (prefix.len() > self.len()) { return false; }
        return __slice(self, 0, prefix.len()) == prefix;
    }
    fn ends_with(self, suffix: string) -> bool {
        if (suffix.len() > self.len()) { return false; }
        return __slice(self, self.len() - suffix.len(), self.len()) == suffix;
    }
    fn index_of(self, needle: string) -> int {
        var last = self.len() - needle.len();
        var at = 0;
        while (at <= last) {
            if (__slice(self, at, at + needle.len()) == needle) { return at; }
            at = at + 1;
        }
        return 0 - 1;
    }
    fn replace(self, needle: string, replacement: string) -> string {
        if (needle.len() == 0) { return self; }
        var out = "";
        var at = 0;
        while (at < self.len()) {
            if (at + needle.len() <= self.len()
                && __slice(self, at, at + needle.len()) == needle) {
                out = out + replacement;
                at = at + needle.len();
            } else {
                out = out + str(self[at]);
                at = at + 1;
            }
        }
        return out;
    }
}
|}

(* The name a prelude span reports. Not a path: nothing reads it, and a
   diagnostic that pointed at a file the user does not have would be worse than
   one that says where it came from. *)
let file = "<prelude>"

(* Parsed once and prepended to the program, so its declarations are in scope
   and go through every pass the program does. *)
let program () =
  match Scanner.scan_tokens ~file source with
  | Error _ -> failwith "the prelude does not scan"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error (e :: _) ->
       failwith (Printf.sprintf "the prelude does not parse [%d:%d] %s" e.Parser.line e.Parser.col e.Parser.message)
     | Error [] -> failwith "the prelude does not parse"
     | Ok program -> program)

