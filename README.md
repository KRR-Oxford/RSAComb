# Combined approach for RSA

Re-implementation of the combined approach for CQ answering over RSA ontologies described in [[1](#references)].

> Please note that the prototype mentioned in [[1](#references)] is not available (and the contributors of this repository have never seen it);
> therefore, this "re-implementation" could be completely different from that prototype (potentially using different tools and programming language).

## Preliminaries

In order to use this program you need to have [RDFox](https://www.oxfordsemantic.tech/product) available in your system, along with a valid license.
RDFox is proprietary software and as such we are not able to distribute it along with our code.
Please refer to [this link](https://www.oxfordsemantic.tech/tryrdfoxforfree) to request a free trial.

This software has been developed and tested with RDFox v.4.0

## Using the software

TODO

### Provide RDFox license

TODO

### Compiling the project

TODO

### Running tests and examples

TODO

## Changes introduced

We tried to implement the system as close as possible to the theoretical description provided in [[1](#references)].
Regardless, we had to deal with the fact that we where using different tools to carry out reasoning tasks and we where probably using a different language to implement the system.
The following is a summary of fixes (🔧), changes (🔄) and improvements (⚡), we introduced along the way:

+ 🔄 [RDFox](https://www.oxfordsemantic.tech/product) is used instead of DLV as the underlying LP engine.

+ 🔄 While the system is described as a two-step process (computing the canonical model and applying the filtering program), the adoption of RDFox allows us to abstract from the underlying engine implementation.
    Materialisation is handled by RDFox, possibly computing answers in one go, without splitting the process in two separate steps.

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
    *Substitution rules have not been entroduced yet.*

+ 🔧 In Def. 4, the definition of built-in predicate `NI` is not consistent with its use in Table 3 and related description in Sec. 4.2.
  We redefined `NI` as the set of all constants that are *equal* to a constant in the original ontology (according to the internal equality predicate `rsa:congruent`).
  Note that, in this scenario, there is no need to introduce `NI` instances as facts in the system;
  instead we can add a rule to populate the new predicate:
  ```
    rsa:NI[?X] :- rsa:congruent[?X, ?Y], rsa:named[?Y] .
  ```
  where `rsa:named` is an internal predicate keeping track of all constants in the original ontology.

## References

[1] Feier, Cristina, David Carral, Giorgio Stefanoni, Bernardo Cuenca Grau, and Ian Horrocks.
    *The Combined Approach to Query Answering Beyond the OWL 2 Profiles*.
    In Proceedings of the Twenty-Fourth International Joint Conference on Artificial Intelligence, IJCAI 2015, Buenos Aires, Argentina, July 25-31, 2015, 2971–2977, 2015.
    http://ijcai.org/Abstract/15/420.

## License

TODO
