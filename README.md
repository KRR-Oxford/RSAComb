# Combined approach for RSA

Re-implementation of the combined approach for CQ answering over RSA ontologies described in [[1](#references)].

> Please note that the prototype mentioned in [[1](#references)] is not available (and the contributors of this repository have never seen it);
> therefore, this "re-implementation" could be completely different from that prototype (potentially using different tools and programming language).

## Preliminaries

In order to use this program you need to have [RDFox](https://www.oxfordsemantic.tech/product) available in your system, along with a valid license.
RDFox is proprietary software and as such we are not able to distribute it along with our code.
Please refer to [this link](https://www.oxfordsemantic.tech/tryrdfoxforfree) to request a free trial.

This software has been developed and tested with RDFox v.4.1

## Using the software

We assume you followed [these steps](https://docs.oxfordsemantic.tech/getting-started.html#getting-started) in order to setup RDFox on your personal machine and in particular you know the path to the `JRDFox.jar` library that comes with the distribution.

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

The output of the command will print the location of the produced jar. Execute it with
```
java -jar <path/to/fat.jar> [<option> ...]
```

Note that the fat jar file distributed with this repository excludes the RDFox as a dependency. Provided that you have the RDFox setup on your machine, you can run the program as follows
```
java -cp <path/to/JRDFox.jar>:<path/to/fat.jar> uk.ac.ox.cs.rsacomb.RSAComb [<option> ...]
```

### Running tests

To run the suit of unit test provided along with the code run
```
sbt test
```

## Changes introduced

We tried to implement the system as close as possible to the theoretical description provided in [[1](#references)].
Regardless, we had to deal with the fact that we where using different tools to carry out reasoning tasks and we where probably using a different language to implement the system.
The following is a (non exhaustive) summary of fixes (🔧), changes (🔄) and improvements (⚡), we introduced along the way:

+ 🔄 [RDFox](https://www.oxfordsemantic.tech/product) is used instead of DLV as the underlying LP engine.

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
        rsa:congruent[?X, ?X] :- owl:Thing[?X] .
        rsa:congruent[?Y, ?X] :- rsa:congruent[?X, ?Y] .
        rsa:congruent[?X, ?Z] :- rsa:congruent[?X, ?Y], rsa:congruent[?Y, ?Z] .
    ```
    defining equivalence as a congruence relation over terms in the ontology.

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

*temporarily redacted*
