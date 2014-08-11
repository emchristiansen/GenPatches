#! /bin/zsh

function run() {
  #cabal clean &&
  sh configure.sh &&
  cabal build
  #cabal haddock
}

while inotifywait -qq -r -e modify .; do echo "Running"; time run; echo "Done"; done
