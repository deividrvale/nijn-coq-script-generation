# ONijn - Coq Proof Script Generation

ONijn is an OCaml module of
[Nijn](https://github.com/nmvdw/Nijn).
It is intended to be used as a tool to generate Coq termination certification
scripts from the output of termination tools.

## Supported rewriting formalisms

The current version of (O)Nijn supports higher-order polynomials
in applicative format.
Essentially,
this is the higher-order interpretation method described in
[Fuhs-Kop](https://drops.dagstuhl.de/opus/volltexte/2012/3492/)'s paper.

## Build Instructions (Linux/MacOS)

Building the onijn package should be pretty easy if you have [opam](https://opam.ocaml.org/doc/Install.html) installed.

- OCaml v4.14.0 or higher.
- Install opam v2.1.3 or higher.
  - See [opam](https://opam.ocaml.org/doc/Install.html) for installation instructions.
- Install dune v3.5.0 or higher.
  - Run ``opam install dune``, it should do the trick.
- To build documentation, we use ``odoc``.
  - Run ``opam install odoc``
- In the root of the project: ``dune build`` to build the package from source.

### Installing binaries locally

To install binaries locally just run ``dune install``.

## How to use it

Onijn receive as input a file describing the term rewriting system and an interpretation of each function symbol in its
signature.
Usually, this is a file in the format ``<file_name>.onijn``.

With onijn installed,
you run it by providing an input file and an output file with the ``-o`` option.

``onijn <input_file> -o <output_file.v>``

The output is a Coq proof script asserting the termination
of the term rewriting system described in the input file.

## Run Experiments

Experiment files are provided in the folder ``./experiments/ho_poly``.

The batch of experiments can be run from the script
``run_experiments.sh`` provided at the root of the project.

**Important:** to run the experiments:

    make
    chmod oug+x run_experiments.sh
    ./run_experiments.sh

## Experimental Setup
