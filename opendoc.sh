#!/bin/bash

# CREDIT TO STAFF OF CS3110 FOR THIS SCRIPT, which was very lightly modified
# from A2 release code

if [[ "$OSTYPE" == "darwin"* ]]; then
  # macOS
  open _build/default/_doc/_html/
elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
  if [[ -n "$IS_WSL" || -n "$WSL_DISTRO_NAME" ]]; then
    # WSL
    DOCPATH=$(wslpath -w ./_build/default/_doc/_html/Chess/Chess/)
    explorer.exe ${DOCPATH} || true
    # Why `|| true`? For unknown reasons, explorer.exe returns error code 1 even
    # when it succeeds in opening the path in a window.
  else
    firefox _build/default/_doc/_html/Chess/Chess/index.html
  fi
fi
