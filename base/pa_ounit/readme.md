% Pa_ounit

Pa_ounit is a syntax extension that helps writing in-line test in ocaml
code. It allows user to register tests with a new `TEST` top-level expressions
and automatically collects all the tests in a module (in a function
`ounit_tests` of type `unit -> OUnit.test`).

Basic case
----------

###prime.ml

    let is_prime = <magic>

    TEST = is_prime 5
    TEST = is_prime 7
    TEST = not (is_prime 1)
    TEST = not (is_prime 8)

###OMakefile

    ....
    OCamlMakePPDeps(pa_ounit,prime)
    ExtractOunitTests(prime)
    ....

This creates a small program `extracted_test_runner.exe` in the same directory
as `prime.ml`.

Using modules and functors
--------------------------

Any test that you add in a submodule declared directly (i.e. using `module .. = struct
.. end`) will be automatically registered in the parent module. Tests in other
modules will need to be manually registered using `TEST_MODULE`.

###tests in a functor.

    module Make(C:S) = struct
         <magic>
         TEST = <some expression>
    end

    TEST_MODULE=Make(Int)

###grouping test and side-effecting initialisation.

Since the module passed as an argument to `TEST_MODULE` is only initialised when
we run the tests. It is therefore ok to perform side-effects in a `TEST_MODULE`

    TEST_MODULE=struct
        module UID = Uniq_id.Int(struct end)

        TEST = UID.create() <> UID.create()
    end

###"piercing" through the signature

When a module contains `TEST` or `TEST_MODULE` the function `ounit_test` is
exported even if the module has a signature that would hide it. Note that this
not apply to modules that do not directly contain a `TEST` or `TEST_MODULE`
statement or to modules that are packed as first class values (e.g. a module
including a module that containing a `TEST` statement).

Grammar
========
The `TEST` and `TEST_MODULE` directive can optionally take a name otherwise the
name of the test is inferred based on the file name and the position in the file.

    | TEST <quoted string>? = <boolean expr>
    | TEST_MODULE <quoted string>? = <module with ounit_tests>
