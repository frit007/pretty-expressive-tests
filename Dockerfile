FROM soraweep/pretty-expressive-oopsla23-artifact:base

###################################################
# Copy Haskell printers, benchmarks, and build them

COPY other-artifacts/ /workspace/other-artifacts
WORKDIR /workspace/other-artifacts

RUN cp -r ../prettiest/Text Text
COPY bernardy-remove-width-limit.patch .
RUN patch -p1 < bernardy-remove-width-limit.patch
RUN mv Text TextPatched

RUN cp -r ../prettiest/Text Text

RUN cabal build

###################################################

WORKDIR /workspace/lean
# merge
COPY lean /workspace/lean
RUN racket scripts/gen-main.rkt

###################################################
RUN apt update
RUN apt upgrade -y
WORKDIR /workspace/pretty-expressive-lean
COPY pretty-expressive-lean /workspace/pretty-expressive-lean
RUN ulimit -s unlimited
RUN lake update
RUN lake build
###################################################
# Clone implementations

WORKDIR /workspace
# version 0.2 is the version used in the paper (and the ABI changes afterwards)
RUN git clone --branch 0.2 https://github.com/sorawee/pretty-expressive-ocaml

# merge
COPY pretty-expressive-ocaml /workspace/pretty-expressive-ocaml

WORKDIR /workspace/pretty-expressive-ocaml

# use the version they used in their paper
RUN opam install -y --working-dir . --with-test
RUN eval $(opam config env) && dune build --release


###################################################
# Copy data

COPY data /workspace/data
ENV BENCHDATA /workspace/data
RUN echo 'export BENCHDATA=/workspace/data' >> ~/.bashrc

COPY scripts /workspace/scripts
COPY benchmark-results /workspace/benchmark-results
COPY output-dir /workspace/output-dir

WORKDIR /workspace

COPY README.md README.md

###################################################
# Done
