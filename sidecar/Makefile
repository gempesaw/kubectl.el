BIN := bin/kubectl-watch-sidecar
SRC := $(shell find . -name '*.go' -not -path './bin/*') go.mod go.sum

# Strip stale GO* env vars; the asdf shim resolves the right toolchain on its own.
GO := env -u GOROOT -u GOPATH -u GOTOOLDIR -u GOMODCACHE -u GOVERSION go

$(BIN): $(SRC)
	@mkdir -p bin
	@$(GO) build -o $(BIN) ./cmd/kubectl-watch-sidecar

run: $(BIN)
	@./$(BIN) $(ARGS)

tidy:
	@$(GO) mod tidy

clean:
	@rm -rf bin

.PHONY: run tidy clean
