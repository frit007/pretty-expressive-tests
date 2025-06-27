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
RUN apt update
RUN apt upgrade -y
WORKDIR /workspace/pretty-expressive-lean
COPY pretty-expressive-lean /workspace/pretty-expressive-lean
RUN ulimit -s unlimited
RUN rm -rf /workspace/pretty-expressive-lean/.lake
RUN lake update
RUN lake build


WORKDIR /workspace/pretty-expressive-lean-no-constraints
COPY pretty-expressive-lean-no-constraints /workspace/pretty-expressive-lean-no-constraints
RUN rm -rf /workspace/pretty-expressive-lean-no-constraints/.lake
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
# Clone Lean batteries
RUN git clone --branch main https://github.com/leanprover-community/batteries.git /workspace/batteries

RUN echo 'export PATH="$HOME/.elan/toolchains/leanprover--lean4---v4.17.0-rc1/bin:$PATH"' >> ~/.bashrc
WORKDIR /workspace/batteries
RUN git reset --hard 80520e5834d0d9a2446cb88ea3d2a38a94d2e143
COPY batteriesLakefile.toml /workspace/batteries/lakefile.toml
COPY ProjectFormat.lean /workspace/batteries/ProjectFormat.lean
RUN lake update
RUN lake build

###################################################
# Clone C++ implementation

RUN apt install g++ -y
RUN git clone --branch main https://github.com/frit007/pretty-expressive-cpp /workspace/pretty-expressive-cpp

WORKDIR /workspace/pretty-expressive-cpp

# compile the C++ benchmarks
RUN g++ sexpr-full.cpp -O3 -o sexpr-full.out
RUN g++ sexpr-random.cpp -O3 -o sexpr-random.out
RUN g++ json.cpp -O3 -o json.out
RUN g++ concat.cpp -O3 -o concat.out
RUN g++ flatten.cpp -O3 -o flatten.out
RUN g++ fill-sep.cpp -O3 -o fill-sep.out
RUN chmod +x run.sh
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
