math-ring
=========

Arithmetic ring typeclass, examples, and utilities.

These modules were extracted from the score generator from my final project for
MUS/ARS 306 at North Carolina State University. As such, the documentation is
pretty limited.

To get an idea of how the Ring typeclass could be implemented, take a look at
`Numeric.Ring.Finite.Modulo`.

To get an idea of how to use the Adjoined type constructor, take a look at
`Numeric.Ring.Finite.Gaussian`. Essentially, you define a dummy type as an
instance of Adjoinable which expresses the rules for how the adjoined element
behaves. You can then use the Adjoined type constructor on it, resulting in a
new ring.


License
-------

This software is available under the LGPL version 3 or later.
