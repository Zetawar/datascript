#!/usr/bin/env bash

docker run \
  -u $(id -u) \
  -e HOME=/datascript \
  -v ${HOME}/.m2:/datascript/.m2 \
  -v $(pwd):/datascript \
  -w /datascript \
  -it \
  djwhitt/lumo \
  /usr/local/bin/lumo -K -D net.cgrand/macrovich:0.2.0 -c src:test:bench/src test_lumo.cljs
