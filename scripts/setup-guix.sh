#!/bin/sh

# https://guix.gnu.org/manual/en/html_node/Binary-Installation.html

set -e

GUIX_TEMP_DIR=$(mktemp -d)
ARCHIVE="$GUIX_TEMP_DIR/guix-binary.tar.xz"
ARCHIVE_URL="https://ci.guix.gnu.org/search/latest/archive?query=spec:tarball+status:success+system:x86_64-linux+guix-binary.tar.xz"

download() {
    curl --silent --show-error --location "$@"
}

download --output "$ARCHIVE" "$ARCHIVE_URL"

cd "$GUIX_TEMP_DIR"
tar --warning=no-timestamp -xf "$ARCHIVE"
mv -v var/guix /var/ && mv -v gnu /

eval 'ROOT_HOME=~root'
GUIX_PROFILE="$ROOT_HOME/.config/guix/current"

mkdir -p "$(dirname "$GUIX_PROFILE")"
ln -sf /var/guix/profiles/per-user/root/current-guix "$GUIX_PROFILE"

# shellcheck source=/dev/null
. "$GUIX_PROFILE/etc/profile"

GUIX_GROUP=guixbuild
groupadd --system "$GUIX_GROUP"
for i in $(seq -w 1 10); do
    useradd -g "$GUIX_GROUP" -G "$GUIX_GROUP" \
        -d /var/empty -s "$(command -v nologin)" \
        -c "Guix build user $i" --system "guixbuilder$i"
done

nohup "$GUIX_PROFILE/bin/guix-daemon" "--build-users-group=$GUIX_GROUP" >/dev/null &

ln -sf /var/guix/profiles/per-user/root/current-guix/bin/guix /usr/local/bin

guix archive --authorize <"$GUIX_PROFILE/share/guix/ci.guix.gnu.org.pub"
guix archive --authorize <"$GUIX_PROFILE/share/guix/bordeaux.guix.gnu.org.pub"
