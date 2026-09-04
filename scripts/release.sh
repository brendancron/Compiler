#!/usr/bin/env bash
#
# Cuts a release from the working tree: sets the version, builds the compiler,
# tags the commit, and publishes the binary as a GitHub release.
#
#   scripts/release.sh v0.2.0
#   scripts/release.sh --dry-run v0.2.0
#
# The tag is the only place the version is typed. `bootstrap/lib/release.ml` is
# where it lives, and this writes it — a binary that reports a version the tag
# does not is one `cx` refuses to hand a job to, and nothing would have caught
# it before someone installed the toolchain.
#
# The bump lands the way every other change does. If `release.ml` does not
# already say what the tag says, this writes it on a branch, opens the pull
# request, and stops: `main` takes no direct pushes, so a script that tried to
# push one would fail after running the whole suite. Merge it and run the same
# command again — the second time the version already matches, and it tags and
# publishes.
#
# `--dry-run` builds and packages, prints the archive and its checksum, and
# stops before tagging, pushing or publishing. The version is set for the build
# and put back afterwards, so the archive is exactly what a real run would ship.
#
# One platform per run — the machine you are on. An OCaml binary is native and
# there is no cross-compilation worth the trouble, so releasing for macOS or
# Windows means running this on one.

set -euo pipefail

root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
cd "$root"

die() { echo "error: $*" >&2; exit 1; }

dry=false
version=""
for arg in "$@"; do
  case "$arg" in
    --dry-run) dry=true ;;
    -*) die "unknown option: $arg" ;;
    *)
      [ -z "$version" ] || die "one version, not two"
      version=$arg
      ;;
  esac
done

[ -n "$version" ] || die "usage: scripts/release.sh [--dry-run] vMAJOR.MINOR.PATCH"
[[ $version =~ ^v[0-9]+\.[0-9]+\.[0-9]+$ ]] || die "'$version' is not vMAJOR.MINOR.PATCH"
bare=${version#v}

if ! $dry; then
  command -v gh >/dev/null || die "gh is not installed"
  gh auth status >/dev/null 2>&1 || die "gh is not logged in"
fi

[ -z "$(git status --porcelain)" ] || die "the working tree has changes"
git rev-parse -q --verify "refs/tags/$version" >/dev/null && die "tag $version already exists"

# The tag has to name a commit the remote already has, or the release points at
# something nobody can check out.
branch=$(git rev-parse --abbrev-ref HEAD)
if ! $dry; then
  git fetch -q origin "$branch" 2>/dev/null || die "origin has no branch '$branch' — push it first"
  [ "$(git rev-parse HEAD)" = "$(git rev-parse FETCH_HEAD)" ] \
    || die "HEAD and origin/$branch differ — push or pull first"
fi

stamp=bootstrap/lib/release.ml
current=$(sed -n 's/^let version = "\(.*\)"$/\1/p' "$stamp")
[ -n "$current" ] || die "cannot read the version out of $stamp"

# The tree was clean coming in, so putting the file back is enough to undo this
# — which a dry run always does, and a real run does only if it fails before
# the commit.
out=""
restore=false
cleanup() {
  [ -n "$out" ] && rm -rf "$out"
  $restore && git checkout -q -- "$stamp"
  return 0
}
trap cleanup EXIT

if [ "$current" != "$bare" ]; then
  echo "==> Version $current -> $bare"
  restore=true
  sed -i.bak "s/^let version = \".*\"$/let version = \"$bare\"/" "$stamp"
  rm -f "$stamp.bak"

  # A dry run wants the version only for the build, and puts it back. A real
  # one has to record it first, and that is a pull request like anything else.
  if ! $dry; then
    onto=release-$bare
    git rev-parse -q --verify "refs/heads/$onto" >/dev/null \
      && die "branch '$onto' already exists — merge or delete it, then run this again"
    echo "==> Opening the pull request that records it"
    git checkout -q -b "$onto"
    git add "$stamp"
    git commit -q -m "release: $bare"
    git push -q -u origin "$onto"
    restore=false
    gh pr create \
      --title "release: $bare" \
      --body "Sets \`$stamp\` to \`$bare\`, so that the binary tagged \`$version\` reports the version its tag names.

Cut with \`scripts/release.sh $version\`, which opens this rather than pushing to \`$branch\` — merge it and run the same command again to tag and publish." \
      >/dev/null
    echo
    echo "==> Merge it, then run: scripts/release.sh $version"
    exit 0
  fi
fi

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
tree="$out/cronyx-$version-$target"
mkdir -p "$tree/bin" "$tree/lib/cronyx"
cp _build/default/cx/bin/main.exe "$tree/bin/cx"
cp _build/default/bootstrap/bin/main.exe "$tree/bin/cronyxc"
chmod +x "$tree/bin/cx" "$tree/bin/cronyxc"
cp -R stdlib "$tree/lib/cronyx/stdlib"

reported=$("$tree/bin/cx" version)
[ "$reported" = "cx $bare" ] || die "the binary reports '$reported', not 'cx $bare'"

archive="cronyx-$version-$target.tar.gz"
tar -czf "$out/$archive" -C "$out" "cronyx-$version-$target"
shasum -a 256 "$out/$archive" | awk '{print $1}' > "$out/$archive.sha256"

if $dry; then
  keep=$(mktemp -d)
  cp "$out/$archive" "$out/$archive.sha256" "$keep/"
  out=""
  echo
  echo "==> Dry run: not tagged, not pushed, not published"
  echo "  $reported"
  echo "  $keep/$archive"
  echo "  sha256 $(cat "$keep/$archive.sha256")"
  exit 0
fi

if $restore; then
  echo "==> Recording the version"
  git add "$stamp"
  git commit -q -m "release: $bare"
  git push -q origin "$branch"
  restore=false
fi

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
