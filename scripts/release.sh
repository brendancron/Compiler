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
(cd bootstrap2 && dune test)

echo "==> Building"
(cd bootstrap2 && dune build --release)

# uname's spelling of the host, in the triple form the archives have always used.
case "$(uname -s)-$(uname -m)" in
  Linux-x86_64)  target=x86_64-unknown-linux-gnu ;;
  Darwin-arm64)  target=aarch64-apple-darwin ;;
  Darwin-x86_64) target=x86_64-apple-darwin ;;
  *) die "no archive name for $(uname -s)-$(uname -m)" ;;
esac

out=$(mktemp -d)
trap 'rm -rf "$out"' EXIT
cp bootstrap2/_build/default/bin/main.exe "$out/cronyxc"
chmod +x "$out/cronyxc"

archive="cronyxc-$version-$target.tar.gz"
tar -czf "$out/$archive" -C "$out" cronyxc
(cd "$out" && sha256sum "$archive" | awk '{print $1}' > "$archive.sha256")

echo "==> Tagging $version"
git tag "$version"
git push origin "$version"

echo "==> Publishing"
gh release create "$version" \
  --title "cronyxc $version" \
  --notes "Download the binary for your platform, unpack it, and put \`cronyxc\` on your \`\$PATH\`.

Run a program with \`cronyxc path/to/file.cx\`. Imports resolve relative to the
file that writes them, so the compiler needs nothing else installed." \
  "$out/$archive" "$out/$archive.sha256"

echo "==> $version published"
