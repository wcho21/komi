build:
  ./bin/build --release

clean:
  ./bin/build clean

test type="unit":
  @if [ "{{type}}" = "unit" ]; then \
    cargo test; \
  elif [ "{{type}}" = "int" ]; then \
    ./bin/test; \
  else \
    echo "Usage: just test [unit|int]"; \
  fi

test-watch:
  ./bin/test-watch

ci:
  ./bin/ci
