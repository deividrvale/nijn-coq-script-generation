# ONijn - Coq Proof Script Generation

ONijn is an OCaml module of
[Nijn](https://github.com/nmvdw/Nijn).
It is intended to be used as a tool to generate Coq termination certification
scripts from the output of termination tools.

The current version of (O)Nijn supports higher-order polynomials in applicative format.
Essentially,
this is the higher-order interpretation method described in
[Fuhs-Kop](https://drops.dagstuhl.de/opus/volltexte/2012/3492/)'s paper.

Please check the [ONijn API](https://deividrvale.github.io/nijn-coq-script-generation/onijn/index.html) for more details.

## Build Instructions (Linux/macOS)

We use [opam](https://opam.ocaml.org/doc/Install.html) to build ONijn.
Make sure it is installed on your system before proceeding.

Here's the dependency list:

- **opam** v2.1.3 or higher.
  - See [opam](https://opam.ocaml.org/doc/Install.html) for installation instructions.
- **ocaml** v4.14.0 or higher.
- **dune** v3.5.0 or higher.
- **menhir** v2.1 or higher.

```bash
opam install dune menhir
```

- To build documentation, we use ``odoc``.

```bash
opam install odoc
```

If your current opam switch doesn't have ocaml v4.14.0 or higher,
we recommend creating a fresh opam switch:

```bash
opam switch create nijn-onijn 4.14.1
eval $(opam env)
opam install dune menhir coq.8.16.1
```

### Managing opam switches

To see the list of switches, use

```bash
opam switch
```

and switching to a new switch is simple.
For instance,

```bash
opam switch nijn-onijn
```



Run

```bash
dune build
```

to build the package from source.

### Installing binaries locally

Run

```ocaml
dune install
```

to install ONijn binaries locally.

## How to use it

Onijn receives as input a file describing the term rewriting system and an interpretation of each function symbol in its
signature.
Usually, this is a file in the format ``<file_name>.onijn``.
The file format is explained in the [API](https://deividrvale.github.io/nijn-coq-script-generation/onijn/index.html#input-file-format).

With onijn installed,
you run it by providing an input file and an output file with the ``-o`` option.

```bash
onijn <input_file> -o <output_file.v>
```

The output is a Coq proof script asserting the termination
of the term rewriting system described in the input file.
This coq proof script can then be verified by coq,
and for that one needs to install the [Nijn](https://github.com/nmvdw/Nijn) coq library.

## Running Experiments

Some experiment files are provided in the folder ``./experiments/ho_poly``.

The batch of experiments can be run from the script
``run_experiments.sh`` provided at the root of the project.

**Important:** to run the experiments make sure the
``timeout`` utility is installed on your system.
On macOS, it is available with ``coreutils`` formulae in brew.

```bash
brew install coreutils
```

Most Linux systems come with timeout by default.

In order to locally run the experiments:

```bash
make
```

this will build and install Nijn on your system.

Next, we set executable permission to the ``run_experiments.sh`` file.

```bash
chmod oug+x run_experiments.sh
```

Finally, execute the ``run_experiments.sh`` file to run the experiments.

```bash
./run_experiments.sh
```
