BINARY  := emacs-installer
PKG     := github.com/Borderliner/emacs-installer
VERSION := $(shell git describe --tags --always --dirty 2>/dev/null || echo dev)
LDFLAGS := -s -w -X main.version=$(VERSION)

.PHONY: all build run vet fmt tidy clean dist

all: build

build:
	go build -ldflags '$(LDFLAGS)' -o $(BINARY) .

run: build
	./$(BINARY)

vet:
	go vet ./...

fmt:
	gofmt -w -l .

tidy:
	go mod tidy

clean:
	rm -f $(BINARY)
	rm -rf dist

# Cross-compile self-contained (CGO-free) binaries for release.
dist: clean
	@mkdir -p dist
	CGO_ENABLED=0 GOOS=linux   GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-linux-amd64   .
	CGO_ENABLED=0 GOOS=linux   GOARCH=arm64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-linux-arm64   .
	CGO_ENABLED=0 GOOS=darwin  GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-darwin-amd64  .
	CGO_ENABLED=0 GOOS=darwin  GOARCH=arm64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-darwin-arm64  .
	CGO_ENABLED=0 GOOS=freebsd GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-freebsd-amd64 .
	CGO_ENABLED=0 GOOS=freebsd GOARCH=arm64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-freebsd-arm64 .
	CGO_ENABLED=0 GOOS=openbsd GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-openbsd-amd64 .
	CGO_ENABLED=0 GOOS=netbsd  GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-netbsd-amd64  .
	@cd dist && sha256sum * > checksums.txt
