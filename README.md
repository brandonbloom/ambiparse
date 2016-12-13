# Ambiparse

Ambiparse is an embedded Clojure DSL that provides parsing for very rich
grammars including ambiguity and adaptability.

## Overview

Ambiparse is capable of parsing any context free language, including
both left-and-right recursive grammars, as well as ambiguous grammars. The
parser is capable of returning a parse tree for ambiguous parses. Furthermore,
grammar productions can be modified during parsing, allowing langauges to
express arbitrary syntax extensions. This means that the parser is actually
capable of parsing many context-sensitive grammars as well.

Ambiguity can be reduced or eliminated from grammars with "disambiguation
filters", which allow for declarative operator associativity and precedence, as
well as filtering by arbitrary predicates.

Being an embedded DSL enables inline semantic actions. Semantic actions can
rewrite _concrete_ syntax trees at parse time in to arbitrary _abstract_
syntax trees, or even perform simple interpretation.

To the best of my knowledge, this confluence of features is novel.

## Usage

Ambiparse is still very early days, so no release has been published, and a
metric ton of documentation is yet to be written. This project was built to
serve a higher-priority project, so it may be a while, if ever, that this
changes.

See the [calculator][1] and [edn parser][2] test code for examples.

The public API is provided by [ambiparse.clj][8] exclusively.

Note that performance is expectedly awful. This too may never change.

## References

- [GLL Parsing][5] by Scott and Johnstone.
- [Disambiguation Filters for Scannerless Generalized LR Parsers][6]
  by M.G.J. van den Brand et al.
- [Faster, Practical GLL Parsing][7] by Afroozeh and Izmaylova.
- [Recursive Adaptable Grammars][9] by John Shutt.

## Acknowledgements

Thanks to Mark Engelberg for [Instaparse][3], an excellent library that you
probably should use over Ambiparse, [his talk][4] which is an excellent
explanation of the GLL algorithm, as well as several excellent discussions
on the finer points of the algorithm's implementation.

## License

Copyright © 2016 Brandon Bloom

Distributed under the Eclipse Public License version 1.0.

[1]: ./test/ambiparse/calc_test.clj
[2]: ./test/ambiparse/edn_test.clj
[3]: https://github.com/Engelberg/instaparse
[4]: https://www.youtube.com/watch?v=b2AUW6psVcE
[5]: http://dotat.at/tmp/gll.pdf
[6]: http://www.st.ewi.tudelft.nl/~eelco/papers/BSVV02.pdf
[7]: http://oai.cwi.nl/oai/asset/24026/24026B.pdf
[8]: ./src/ambiparse.clj
[9]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.45.2424&rep=rep1&type=pdf
