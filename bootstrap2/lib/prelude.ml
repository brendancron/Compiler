(* Declarations every program starts with, parsed and checked with it. Nothing
   here is reachable from the compiler save for the three types `typeof`
   answers with. It becomes a file the module loader reads once there is one. *)

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

// What `a[i:j]` puts between the brackets. The four shapes are four variants
// rather than one pair with sentinels, so a missing bound is missing rather
// than encoded.
type Range {
    Between(int, int),
    From(int),
    To(int),
    All
}

// A bound counted from the end is resolved here, once, where the length is
// known — which is why it is the entry's business and not the language's.
fn __bound(at: int, length: int) -> int {
    var resolved = at;
    if (resolved < 0) { resolved = length + resolved; }
    if (resolved < 0) { resolved = 0; }
    if (resolved > length) { resolved = length; }
    return resolved;
}

fn __span(r: Range, length: int) -> (int, int) {
    match r {
        Range::Between(from, to) => {
            return (__bound(from, length), __bound(to, length));
        }
        Range::From(from) => { return (__bound(from, length), length); }
        Range::To(to) => { return (0, __bound(to, length)); }
        Range::All => { return (0, length); }
    }
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
// Slicing is indexing by a range, so it is the same operator over a different
// index — and it copies, because a view would need to say how long it lives.
op []<T>(self: List<T>, r: Range) -> List<T> {
    var bounds = __span(r, self.len());
    var out: List<T> = [];
    var at = bounds.0;
    while (at < bounds.1) {
        out.push(self[at]);
        at = at + 1;
    }
    return out;
}

op []<T>(self: Array<T>, r: Range) -> Array<T> {
    var bounds = __span(r, self.len());
    var taken = bounds.1 - bounds.0;
    if (taken <= 0) { return new Array<T>(0, self[0]); }
    var out = new Array<T>(taken, self[bounds.0]);
    var at = 0;
    while (at < taken) {
        out[at] = self[bounds.0 + at];
        at = at + 1;
    }
    return out;
}

op [](self: string, r: Range) -> string {
    var bounds = __span(r, self.len());
    return __slice(self, bounds.0, bounds.1);
}

|}

(* Not a path: a diagnostic pointing at a file the user does not have would be
   worse than one that says where it came from. *)
let file = "<prelude>"

(* Scanned and parsed once. Metaprocessing compiles the declarations around
   every meta block and every meta call site, so this is asked for once per
   such site rather than once per program. Sharing one tree is safe because
   nothing after this point mutates it. *)
let parsed =
  lazy
    (match Scanner.scan_tokens ~file source with
  | Error _ -> failwith "the prelude does not scan"
  | Ok tokens ->
    (match Parser.parse tokens with
     | Error (e :: _) ->
       failwith (Printf.sprintf "the prelude does not parse [%d:%d] %s" e.Parser.line e.Parser.col e.Parser.message)
     | Error [] -> failwith "the prelude does not parse"
     | Ok program -> program))

let program () = Lazy.force parsed

