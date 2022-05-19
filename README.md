<!-- TITLE -->
<p align="center">

<!--
<a href="https://github.com/">
<img src="resources/logo.png" alt="Logo" width="80" height="80">
</a>
-->

<h1 align="center">&middot; RSAComb &middot;</h1>

<p align="center">
Combined approach for Conjunctive Query answering in RSA
<br/>
<a href="https://arxiv.org/abs/2107.00369">Read the paper</a>
&middot;
<a href="http://ceur-ws.org/Vol-2954/paper-18.pdf">Read the technical report</a>
&middot;
<a href="https://github.com/KRR-Oxford/RSAComb/issues">Report bug</a>
<br/><br/>
<a href="https://github.com/KRR-Oxford/RSAComb/releases/latest">
    <img src="https://img.shields.io/github/release/KRR-Oxford/RSAComb.svg?style=for-the-badge" alt="Release badge">
</a>
<a href="https://github.com/KRR-Oxford/RSAComb/issues">
    <img src="https://img.shields.io/github/issues/KRR-Oxford/RSAComb.svg?style=for-the-badge" alt="Issues badge">
</a>
<a href="https://github.com/KRR-Oxford/RSAComb/actions">
    <img src="https://img.shields.io/github/workflow/status/KRR-Oxford/RSAComb/Scala%20CI/develop?label=TESTS&style=for-the-badge" alt="GitHub Actions badge">
</a>
<a href="LICENSE">
    <img src="https://img.shields.io/github/license/KRR-Oxford/RSAComb.svg?style=for-the-badge" alt="License badge">
</a>
<a href="https://doi.org/10.5281/zenodo.5047810">
    <img src="https://img.shields.io/badge/DOI-10.5281/zenodo.5047810-blue?style=for-the-badge" alt="DOI badge">
</a>
</p>

</p>

## About

This is an *improved* re-implementation of the combined approach for CQ answering over RSA ontologies described in [[1](#references)].

> Please note that the prototype mentioned in [[1](#references)] is not available (and the contributors to this repository have never seen it);
> therefore, this "re-implementation" could be completely different from that prototype (potentially using different tools and programming language).

## Preliminaries

In order to use this program you need to have [RDFox](https://www.oxfordsemantic.tech/product) available in your system, along with *a valid license*.
RDFox is proprietary software and as such we are not able to distribute it along with our code.
Please refer to [this link](https://www.oxfordsemantic.tech/tryrdfoxforfree) to request a free trial.

This software has been developed and tested with RDFox v5.5

## Changes introduced

We tried to implement the system as close as possible to the theoretical description provided in [[1](#references)].
Regardless, we had to deal with the fact that we are using different tools to carry out reasoning tasks and we are probably using a different language to implement the system.
The following is a (non exhaustive) summary of fixes (🔧), changes (🔄) and improvements (⚡), we introduced along the way:

+ 🔄 [RDFox](https://www.oxfordsemantic.tech/product) is used instead of DLV as the underlying LP engine.

+ ⚡ The system accepts unrestricted OWL ontologies as input and takes care of normalising and approximating the ontology to RSA.
    At the time of writing, two approximation algorithms are provided, to compute a sound (or complete) set of answer to the input queries, respectively.

+ ⚡ The different steps of the combined approach (namely, the canonical model computation and the filtering step) are executed in isolation using different *named graphs*.
    This allows us to reuse partial products of the computation and can even be used to parellalise filtering and answering steps.

+ 🔧 In Def.4, the definition of built-in predicate `notIn` is wrong and should reflect the implicit semantics implied by the name, i.e.,

    > let [...] `notIn` be a built-in predicate which holds when the first argument is **not** an element of the set given as second argument

    This has been fixed by (1) introducing a built-in predicate `In` (note that instances of `In` can be computed beforehand since they only depend on the input ontology), and (2) implement `notIn` as the negation of `In` using RDFox *NaF* built-in support.

+ 🔄 Top (`owl:Thing`) axiomatisation is performed introducing rules as follows.
    Given `p` predicate (arity *n*) *in the original ontology*, the following rule is introduced:
    ```
        owl:Thing[?X1], ..., owl:Thing[?Xn] :- p(?X1, ..., ?Xn) .
    ```
    Note that, by definition, arity can be either 1 or 2.

+ 🔄 Equality axiomatisation is performed introducing the following rules:
    ```
        rsacomb:congruent[?X, ?X] :- owl:Thing[?X] .
        rsacomb:congruent[?Y, ?X] :- rsacomb:congruent[?X, ?Y] .
        rsacomb:congruent[?X, ?Z] :- rsacomb:congruent[?X, ?Y], rsacomb:congruent[?Y, ?Z] .
    ```
    defining equivalence as a congruence relation over terms in the ontology.
    Substitution rules propagate the equivalence to all existing atoms.

+ 🔧 In Def. 4, the definition of built-in predicate `NI` is not consistent with its use in Table 3 and related description in Sec. 4.2.
  We redefined `NI` as the set of all constants that are *equal* to a constant in the original ontology (according to the internal equality predicate `rsa:congruent`).
  Note that, in this scenario, there is no need to introduce `NI` instances as facts in the system;
  instead we can add a rule to populate the new predicate:
  ```
    rsa:NI[?X] :- rsa:congruent[?X, ?Y], rsa:named[?Y] .
  ```
  where `rsa:named` is an internal predicate keeping track of all constants in the original ontology.

+ ⚡ In Def. 3, regarding the generation of the logic program used for the RSA check, only T5 axioms involving an unsafe role will introduce the internal predicates `PE` and `U`.

+ ⚡ Both in the canonical model and the filtering program computations, rules without a body are loaded into RDFox as facts.

+ ⚡ The `cycle` function introduced in Def.4 establishing the direction of the *unraveling* of loops is defined over triples `(A,R,B)`. We are currently limiting the triple only to those appearing in a T5 axiom `A ⊑ ∃R.B`. Note that this greatly limits the size of cycle for a given triple, and as a consequence limits the number of rules used to compute the canonical model.

## Using the software

We assume you followed [these steps](https://docs.oxfordsemantic.tech/getting-started.html#getting-started) in order to setup RDFox on your personal machine and in particular you know the path to the `JRDFox.jar` library that comes with the distribution.

Alternatively, run the following commands from the root of the project to install RDFox locally.
Download links for specific versions and operating systems can be found [here](https://www.oxfordsemantic.tech/downloads).

```{.bash}
mkdir -p lib && pushd lib
wget https://rdfox-distribution.s3.eu-west-2.amazonaws.com/release/v5.2.1/RDFox-linux-x86_64-5.2.1.zip
unzip RDFox-linux-x86_64-5.2.1.zip
ln -s RDFox-linux-x86_64-5.2.1.zip/lib/JRDFox.jar
popd
```

### Provide RDFox license

The [documentation](https://docs.oxfordsemantic.tech/features-and-requirements.html#license-key), describes several ways to provide the license to RDFox.

One easy way is to put your license key in a file `RDFox.lic` in `$HOME/.RDFox/`, with adequate read permissions for the user executing the program.

### Compiling and running the project

The project uses [sbt](https://www.scala-sbt.org/) to manage dependences.

To compile the project run the following from the base directory:
```
sbt compile
```

The project uses the sbt plugin [sbt-assembly](https://github.com/sbt/sbt-assembly) to produce a fat jar with all the required dependences.
Run the following from the base directory of the project to produce a standalone `jar` file.
```
sbt assembly
```

The output of the command will print the location of the produced jar.
Note that the fat jar file distributed with this repository excludes the RDFox as a dependency. Provided that you have the RDFox setup on your machine, you can run the program as follows
```
java -cp <path/to/JRDFox.jar>:<path/to/fat.jar> uk.ac.ox.cs.rsacomb.RSAComb [<option> ...]
```

### Running tests

To run the suites of tests provided along with the code run
```
sbt test
```

This will run all [unit tests](https://en.wikipedia.org/wiki/Unit_testing) and [functional tests](https://en.wikipedia.org/wiki/Functional_testing).
If you want to limit the scope of the tests and run only a particular suite use
```
sbt "testOnly <test-class>"
```

For example, to execute only unit tests concerning the canonical model computation, run
```
sbt "testOnly uk.ac.ox.cs.rsacomb.CanonicalModelSpec"
```

or alternatively
```
sbt "testOnly *CanonicalModelSpec"
```

To run only functional tests for LUBM, excluding tests tagged as *slow* (that require more resources), run
```
sbt "testOnly *functional.LUBM -- -l org.scalatest.tags.Slow"
```

### Debugging

You can set the logging level of RSAComb using the `-l | --logger` flag (see the help screen for more information).
When the logger is set to `verbose`, RSAComb will generate a set of files that contain the intermediate products of the program execution (these include the set of rules to generate the canonical model for the input ontology and the filtering rules derived from the input query).
These files are stored in the working directory, in a new folder named `rsacomb-<timestamp>`.

You can load these files directly into RDFox to simulate the same environment used by RSAComb, leaving you in a state just before the answer gathering process.
We also provide a convenient `simulate.rdfox` RDFox script that can be used to load all the necessary files in RDFox for you.

Let's suppose you run the following command from the root of the project
```{.sh}
java -cp lib/JRDFox.jar:target/scala-2.13/RSAComb-assembly-1.1.0.jar uk.ac.ox.cs.rsacomb.RSAComb -l verbose -o tests/lubm/univ-bench.owl -d tests/lubm/data/lubm1.ttl -q tests/lubm/queries.sparql
```

This will answers all the queries in `tests/lubm/queries.sparql` and generate debug information in a new folder in the current working directory (let's say, `rsacomb-20211005120845/`).
You can run the provided RDFox script as follows
```{.sh}
path/to/RDFox sandbox <debug-folder> "simulate <query-id>"
```
where
- `debug-folder` is the newly generated folder (`rsacomb-20211005120845` in this example)
- `query-id` is the identifier of the query we want to simulate (if we want to simulate query 16 we will pass `16` as an argument). We can pass `all` to simulate all queries.

This will launch a sandboxed RDFox console, where you will be able to explore a simulation of the datastore used by RSAComb.
You can also access the same datastore from the web interface at [http://localhost:12110/console/](http://localhost:12110/console/).

## References

[1] Feier, Cristina, David Carral, Giorgio Stefanoni, Bernardo Cuenca Grau, and Ian Horrocks.
    *The Combined Approach to Query Answering Beyond the OWL 2 Profiles*.
    In Proceedings of the Twenty-Fourth International Joint Conference on Artificial Intelligence, IJCAI 2015, Buenos Aires, Argentina, July 25-31, 2015, 2971–2977, 2015.
    http://ijcai.org/Abstract/15/420.

[2] Horridge, Matthew and Bechhofer, Sean.
    *The OWL API: A Java API for OWL Ontologies*.
    Semantic Web Journal 2(1), Special Issue on Semantic Web Tools and Systems, pp. 11-21, 2011.

## Acknowledgements

- OWLAPI [[2]](#references)
- [RDFox](https://www.oxfordsemantic.tech/product)
- [Graph for Scala](https://github.com/scala-graph/scala-graph)

## Credits

- Federico Igne
- Stefano Germano
- Ian Horrocks (*Scientific Supervisor*)

From the [Knowledge Representation and Reasoning research group](https://www.cs.ox.ac.uk/isg/krr/) in the [Department of Computer Science](https://www.cs.ox.ac.uk/) of the [University of Oxford](https://www.ox.ac.uk/).

## License

This project is licensed under the [Apache License 2.0](LICENSE).
