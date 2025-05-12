build:
  ./bin/build.sh --release

clean:
  ./bin/build.sh clean

test:
  ./bin/test.sh

ci:
  ./bin/ci.sh