#!/usr/bin/env bash
#
# Cuts a release from the working tree: builds the compiler, tags the commit,
# and publishes the binary as a GitHub release.
#
#   scripts/release.sh v0.2.0
#
# One platform per run — the machine you are on. An OCaml binary is native and
# there is no cross-compilation worth the trouble, so releasing for macOS or
# Windows means running this on one.

set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"

die() { echo "error: $*" >&2; exit 1; }

version=${1:-}
[ -n "$version" ] || die "usage: scripts/release.sh vMAJOR.MINOR.PATCH"
[[ $version =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]] || die "'$version' is not vMAJOR.MINOR.PATCH"

command -v gh >/dev/null || die "gh is not installed"
gh auth status >/dev/null 2>&1 || die "gh is not logged in"

[ -z "$(git status --porcelain)" ] || die "the working tree has changes"
git rev-parse -q --verify "refs/tags/$version" >/dev/null && die "tag $version already exists"

# The tag has to name a commit the remote already has, or the release points at
# something nobody can check out.
branch=$(git rev-parse --abbrev-ref HEAD)
git fetch -q origin "$branch" 2>/dev/null || die "origin has no branch '$branch' — push it first"
[ "$(git rev-parse HEAD)" = "$(git rev-parse FETCH_HEAD)" ] \
  || die "HEAD and origin/$branch differ — push or pull first"

echo "==> Testing"
(cd bootstrap && dune test)
dune test cx
./scripts/cx-parity.sh

echo "==> Building"
dune build --release

# uname's spelling of the host, in the triple form the archives have always used.
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64)  target=x86_64-unknown-linux-gnu ;;
  Darwin-arm64)  target=aarch64-apple-darwin ;;
  Darwin-x86_64) target=x86_64-apple-darwin ;;
  *) die "no archive name for $(uname -s)-$(uname -m)" ;;
esac

# One release ships one toolchain: the package manager, the compiler it links,
# and the standard library it resolves `std/…` against. `cx` finds the library
# at ../lib/cronyx/stdlib, so the layout here is the layout it is installed in.
out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
tree="$out/cronyx-$version-$target"
mkdir -p "$tree/bin" "$tree/lib/cronyx"
cp _build/default/cx/bin/main.exe "$tree/bin/cx"
cp _build/default/bootstrap/bin/main.exe "$tree/bin/cronyxc"
chmod +x "$tree/bin/cx" "$tree/bin/cronyxc"
cp -R stdlib "$tree/lib/cronyx/stdlib"

archive="cronyx-$version-$target.tar.gz"
tar -czf "$out/$archive" -C "$out" "cronyx-$version-$target"
shasum -a 256 "$out/$archive" | awk '{print $1}' > "$out/$archive.sha256"

echo "==> Tagging $version"
git tag "$version"
git push origin "$version"

echo "==> Publishing"
gh release create "$version" \
  --title "cronyx $version" \
  --notes "Unpack it and put \`bin/cx\` on your \`\$PATH\`, keeping \`lib/cronyx/stdlib\`
beside it — that is where \`import \"std/…\"\` resolves to.

    cx new hello
    cd hello
    cx run" \
  "$out/$archive" "$out/$archive.sha256"

echo "==> $version published"

# What the Homebrew formula needs, so cutting a release hands it over rather
# than leaving someone to compute it.
echo
echo "For brendancron/homebrew-cronyx, $target:"
echo "  url \"https://github.com/brendancron/CronyxLang/releases/download/$version/$archive\""
echo "  sha256 \"$(cat "$out/$archive.sha256")\""
