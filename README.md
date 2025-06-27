## Introduction 

We extend Pretty Expressive with context sharing. To demonstrate that these extensions introduce minimal overhead, we compare our implementation with the original. Since programming language differences can impact performance, we also created an OCaml-style version of the implementation to isolate those effects. Additionally, to illustrate how performance can depend heavily on data access patterns, we developed a C++ version using data-oriented design. This version shows that memory-intensive tests can be accelerated by 3–4×, primarily through array reuse and memory arenas.

This repository and Docker setup is a fork of the original papers setup for testing the implementation.

### Implementations 
We Added a C++ and a Lean implementation of Pretty Expressive, as well as our extension to pretty expressive. 
Sorawee created the original Racket and Ocaml implementation. We added them to this repository to compare all versions of Pretty Expressive.

| Name                                   | Path                                                     |
|----------------------------------------|----------------------------------------------------------|
| OCaml PrettyExpressive pretty printer  | `/workspace/pretty-expressive-ocaml/lib/`                |
| Racket PrettyExpressive pretty printer | `/workspace/pretty-expressive-racket/`                   |
| Lean PrettyExpressive pretty printer   | `/workspace/pretty-expressive-lean-no-constraints/`      |
| Lean PrettyExpressive+ pretty printer  | `/workspace/pretty-expressive-lean/`                     |
| C++ PrettyExpressive pretty printer    | `/workspace/pretty-expressive-cpp/ `                     |
| Racket code formatter                  | `/workspace/fmt/`                                        |

### Hardware requirements

To use this artifact, you will need a machine capable of running Docker with at least 15 GB of free disk space. 

For reference, we tested this artifact on a windows machine with 32GB of RAM

### Installation

The only required installation is Docker. See https://docs.docker.com/install/ for details on how to install Docker.

After installing Docker, you can download and run the Docker image with (possibly with `sudo` if required):

```
# Download image (~15 GB download). 
$ docker pull frit007/pretty-expressive-artifact:latest
# Run the container
$ docker run -it --name pretty-artifact frit007/pretty-expressive-artifact
```

This will drop you into a shell inside the container, in the `/workspace` directory, the main directory containing Lean proofs, implementations, and benchmarks.

We have installed `vim` into the container for convenience to edit and view files; you can also use Docker to copy files into and out of the container, see <https://docs.docker.com/engine/reference/commandline/cp/>.

If you leave the container and wish to get back, you will need to restart it with the command `docker start -ia pretty-artifact`. If you wish to instead start from a fresh copy of the image, run `docker rm pretty-artifact` to remove the container and then repeat `docker run -it --name pretty-artifact frit007/pretty-expressive-artifact` to start a new container.


### Evaluation: Formatting batteries
Used to evaluate how far we have come with the evaluation of batteries

```
cd batteries
lake exe ProjectFormat -folder ../batteries/Batteries -include ../batteries/.lake/build/lib  -include .lake/build/lib -filesPrWorker 1 -workers 16
```
and look for the line
```
Managed to format {var} out of commands {var}
Time readAndParse: {var} pf: {var} doc: {var} reparse: {var}
```

The formatting of a single file tests were performed manually, you can get the correspoding windows setup with 
```
./batteries_setup.bat
```
import the LSPformat to get access to the formatter and enable time debugging. Disable or enable debugMode based on the test.
```
import LSPformat
set_option pf.debugTime true
set_option pf.debugMode true -- or false
```

### Evaluation: benchmarks 

(Expected execution time: ~35 minutes)

This section aims to reproduce/generate Table 2 from the Pretty expressive paper.

First, clear any existing data points.

```
cd /workspace 
rm benchmark-results/*.dat 
```

To generate data points for Table 2, run:

```
racket scripts/gen-table-2.rkt
```


The table can then be viewed with:

```
$ racket scripts/view-tables.rkt
```

