#!/bin/bash

# Resize and link photos
fullpath="$1"
basedir=$(dirname "$fullpath")
fname=$(basename "$fullpath")
size=$(identify "$fname" | awk '{print $3}' |awk -Fx '{print ($1 > $2)? 2880 : 1920};')
mkdir -p "$basedir/resized" "$basedir/best"
ln -fs "$fullpath" "$basedir/best"
convert "$fullpath" -resize $size -quality 82 -depth 8 "$basedir/resized/$fname"
