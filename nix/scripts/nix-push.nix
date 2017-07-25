{ writeScript, bash, google-cloud-sdk, awscli, nix }:
writeScript "nix-cloud-push" ''
#!${bash}/bin/bash
#
# This script is used by the CI to push derivation outputs to the binary cache
#
# Usage:
#   BUCKET_URI=gs://my-nix-cache \
#   NIX_KEY_FILE=./mysecret \
#   GOOGLE_APPLICATION_CREDENTIALS=path/to/account.json \
#   NIX_PATH="nixpkgs=path/to/top-level/nix-directory"
#   nix-push $(nix-build nix -A backend)
set -euo pipefail

# https://developers.google.com/identity/protocols/application-default-credentials

cache_dir=''${CACHE_DIR:-/tmp/nix-binary-cache-dir}
bucket_uri=''${BUCKET_URI:-gs://my-binary-cache/}
# Generated using:
#   nix-store --generate-binary-cache-key cache.mydomain.com-1 sk pk
key_file=''${NIX_KEY_FILE:-/run/keys/nix/sk}

# Prepare the NAR files
${nix}/bin/nix-push --key-file "$key_file" --dest "$cache_dir" "$@"

case "$bucket_uri" in
  gs://*)
    ${google-cloud-sdk}/bin/gsutil -m rsync -a public-read -c -C -r "$cache_dir" "$bucket_uri"
    ;;
  s3://*)
    ${awscli}/bin/aws s3 sync "$cache_dir" "$bucket_uri" --acl public-read --size-only --cache-control 'max-age=365000000, immutable'
    ;;
  *)
    echo "FAIL: Unknown bucket URI '$bucket_uri'"
    exit 1
esac
''
