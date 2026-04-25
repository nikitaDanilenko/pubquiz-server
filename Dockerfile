FROM haskell:9.10.3 AS builder

WORKDIR /app

# Install system dependencies for PostgreSQL
RUN apt-get update && apt-get install -y libpq-dev && rm -rf /var/lib/apt/lists/*

# Copy Stack files first for better caching
COPY stack.yaml package.yaml ./

RUN stack build --only-dependencies --system-ghc

COPY . .
RUN stack build --system-ghc --copy-bins --local-bin-path /app/bin

FROM debian:bullseye-slim

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq5 ca-certificates && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/bin/pubquiz-server /app/pubquiz-server
COPY --from=builder /app/config /app/config

CMD ["/app/pubquiz-server"]
