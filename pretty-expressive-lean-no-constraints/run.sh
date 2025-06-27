#!/usr/bin/env sh

# Get the base name of the file without the .ml extension
exe=${1%}

# Apply specific string mappings
case "$exe" in
  concat) exe="Concat" ;;
  fill_sep) exe="FillSep" ;;
  flatten) exe="Flatten" ;;
  json) exe="Json" ;;  
  sexp_full) exe="SExpFull" ;;
  sexp_random) exe="SExpRandom" ;;
esac

# Shift positional parameters to exclude the first argument
shift

ulimit -s unlimited

# Run the command with the transformed exe name
".lake/build/bin/$exe" "$@"