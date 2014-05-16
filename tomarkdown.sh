#!/bin/sh

SOURCE="$1"
TARGET="$2"

cp "$SOURCE" "$TARGET"
sed -i 's/{-/```/g' "$TARGET"
sed -i 's/-}/\n```Idris/g' "$TARGET"
