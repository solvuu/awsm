SHELL=/bin/bash

.PHONY: default
default: dune-project start-ocaml

################################################################################
# Developer commands

.PHONY: install-deps
install-deps:
	test -d _opam || opam switch create . 4.14.1 --no-install --yes
	( eval $$(opam env) && opam install . --deps-only --yes )
	( eval $$(opam env) && opam install ocamlformat ocaml-lsp-server utop --yes )

.PHONY: start-ocaml
start-ocaml:
	dune build @all -w

# start-ocaml builds all packages. That can be slow. The following command builds a
# single package. For example, try `make aws-s3`.
aws-%:
	dune build @aws/$*/all -w

dune-project: FORCE
	dune exec bin/awsm_build.exe -- build-dune-project --botocore-data vendor/botocore/botocore/data

botodata-%:
	wget https://github.com/boto/botocore/archive/$*.tar.gz
	tar xzf $*.tar.gz
	rm -f $*.tar.gz
	mkdir -p vendor/botocore/botocore
	cp -Rpifn botocore-$*/botocore/data vendor/botocore/botocore/
	cp -fn botocore-$*/CONTRIBUTING.rst botocore-$*/LICENSE.txt botocore-$*/NOTICE botocore-$*/README.rst vendor/botocore/
	rm -rf botocore-$*
	git add vendor

.PHONY: doc
doc:
	dune build @doc

# Build docker image for given library. For example `make
# docker-async` will build an image called awsm-async.
docker-%:
	./bin/make_dockerfile.ml $* > Dockerfile.$*
	docker build -t awsm-$* -f Dockerfile.$* .
	rm -f Dockerfile.$*

.PHONY: format
format:
	dune fmt

################################################################################
# Tests
.PHONY: runtest
runtest:
	dune build @runtest

################################################################################
# Production build
.PHONY: build-services
build-services:
	dune exec app/codegen/awsm_codegen.exe -- services --botocore-data vendor/botocore/botocore/data -o aws

################################################################################
# Cleanup
.PHONY: clean
clean:
	dune clean

FORCE:
