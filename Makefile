BINARY  := emacs-installer
PKG     := github.com/hajianpour/emacs-installer
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

# Cross-compile static-ish binaries for release.
dist: clean
	@mkdir -p dist
	GOOS=linux  GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-linux-amd64  .
	GOOS=linux  GOARCH=arm64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-linux-arm64  .
	GOOS=darwin GOARCH=amd64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-darwin-amd64 .
	GOOS=darwin GOARCH=arm64 go build -ldflags '$(LDFLAGS)' -o dist/$(BINARY)-darwin-arm64 .
