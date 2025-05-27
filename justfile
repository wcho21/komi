build:
  ./bin/build --release

clean:
  ./bin/build clean

test type="all":
  @if [ "{{type}}" = "unit" ]; then \
    cargo test; \
  elif [ "{{type}}" = "int" ]; then \
    ./bin/test; \
  elif [ "{{type}}" = "all" ]; then \
    cargo test && ./bin/test; \
  else \
    echo "Usage: just test [unit|int]"; \
  fi

test-watch:
  ./bin/test-watch

ci:
  ./bin/ci
