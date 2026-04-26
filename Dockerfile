FROM haskell:9.10.3-bookworm AS builder

WORKDIR /app

# Install system dependencies for PostgreSQL
RUN apt-get update && apt-get install -y libpq-dev pkg-config && rm -rf /var/lib/apt/lists/*

# Copy Stack files first for better caching
COPY stack.yaml stack.yaml.lock package.yaml ./

RUN stack build --only-dependencies --system-ghc --no-library-profiling

COPY . .
RUN stack build --system-ghc --copy-bins --local-bin-path /app/bin --no-library-profiling

FROM debian:bookworm-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/bin/pubquiz-server /app/pubquiz-server
COPY --from=builder /app/config /app/config

CMD ["/app/pubquiz-server"]
