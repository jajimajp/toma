> [!NOTE]
> This software is a customized version of Toma, designed to produce output that is easier to parse.
> The original version can be found here: https://www.jaist.ac.jp/project/maxcomp.

# Toma

Toma is a theorem prover for first-order equational systems,
the successor of Tofu.

## How to build

1. Install:

- GNU/Make
- Glasgow Haskell Compiler 9.2.5 (or higher)
- Z3 version 4.8.12 (or higher)

2. `$ make` produces the executable `toma`.

## How to run

Toma accepts TPTP format https://www.tptp.org/ .
For example,

    $ ./toma commutation.p
    ...
    % SZS status Satisfiable

shows the commutation law `x * y = y * x` is not valid in the theory of group.
For another example,

    $ ./toma inverse_unit.p
    ...
    % SZS status Unsatisfiable

shows `e^(-1) = e` is valid in the theory of group.

For the help message, type `$ ./toma -h`.

## What is changed from original version

This customized version (v0.6+PARSABLE) comes with `--completion-with-parsable-output` option.

```bash
./toma --completion-with-parsable-output examples/group/group.trs
```

## How to make StarExec package

To run Toma on StarExec, every binary must be statically linked.

### Build Toma for StarExec

1. Install the traditional GNU linker `ld.bfd`, for instance type
   `$ sudo apt install binutils-i586-linux-gnu`.
2. `$ make starexec`
3. Check if `toma` is produced and statically linked:

```
$ file toma
toma: ELF 64-bit LSB executable, x86-64, version 1 (GNU/Linux), statically linked, BuildID[sha1]=2665a2592c80b02dbd5e0dfe99844b4040c0e625, for GNU/Linux 3.2.0, with debug_info, not stripped
```

### Build Z3 for StarExec

0. Install cmake.
1. Download Z3 4.8.17 source code (either of the last two in the assets list) from
   https://github.com/Z3Prover/z3/releases/tag/z3-4.8.17 .
2. Extract the archive, and

```
   $ cd z3-4.8.17
   $ mkdir build
   $ cd build
   $ cmake .. -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXE_LINKER_FLAGS:STRING='-static -Wl,--whole-archive -lrt -lpthread -Wl,--no-whole-archive'
   $ make -j4 # compilation takes long time
```

3. Check if `z3` is produced and statically linked:

```
$ file z3
z3: ELF 64-bit LSB executable, x86-64, version 1 (GNU/Linux), statically linked, BuildID[sha1]=95991503c5806fda5744cbbbd78f8320cc126917, for GNU/Linux 3.2.0, not stripped
```

### Packaging

For example, we can make the package for CoCo as follows.

0. Make the following directry structure, and copy toma and z3 to the directry.

```
Toma/
├── bin
│   ├── LICENSE_Z3
│   ├── starexec_run_default
│   ├── toma
│   └── z3
└── starexec_description.txt
```

1. `$ cd Toma`
2. `$ zip -r Toma.zip *`
3. Upload `Toma.zip` to StarExec.

## Authors

- Teppei Saito
- Nao Hirokawa

## License

Toma is released under the MIT License. See LICENSE.txt.
