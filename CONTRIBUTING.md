# How to contribute

Thank you for taking the time to contribute to `TorXakis`. We value your
effort, and to make sure that it remains a quality tool that can be extended
and maintained by the community we require that certain requirements are
satisfied before incorporating your changes.

## Adding new features

When adding a new feature, please make sure that it is:

- [ ] Properly documented: whenever applicable, there should be a description
  of the syntax and semantics of the new feature (in the form of SOS rules for
  instance), links to literature that contain the theoretical foundations
  behind the new feature, and extensive examples on how to use it.
- [ ] Properly tested: there should
  be [integration or unit tests](#integration-and-unit-tests) for the new
  feature.
- [ ] Properly benchmarked: there should be benchmarks that measure the
  performance of the new feature, and how the new feature affects the existing
  benchmarks. Ideally it should be no performance degradation.
- [ ] Built fast on CI: as a rule of thumb, the build time on CI should be no
  greater than 5 minutes (taking our fastest
  CI, [Semaphore](http://semaphoreci.com/)). If it takes more, please provide a
  description of why this is needed.
- [ ] Adhering to our [code style](#code-style). There is an [automatic
  pretty printer](https://github.com/commercialhaskell/hindent) that can be
  used to format the code.

## Unit and Integration tests

_Unit Tests_ are used for testing the **smallest possible meaningfully-functional** unit of code.
In Haskell’s case, this unit of code would be a function that is simple enough to handle only a small part of a functionality.

_Integration Tests_ are big brothers of Unit Tests:
They are written and run the same way, but they cover a **multitude of units or functionalities** within the package.

Typically a feature development tast would require _multiple unit tests_ and _at least one integration test_ to be added or changed.

Make sure that you update the tests as necessary and run the full test suite successfully before submitting your change request.
Overall code coverage should not decrease with your contribution.

**Change or removal of a test should always be justified by either test being wrong or a behaviour/requirement being changed.**

## Coding style

We
use
[Johan Tibell](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md) style
guide because:

- It is a lightweight style guide.
- It is quite permissive, but also sensible.
- It includes useful guidelines for avoiding unnecessary lazy constructor
  fields.

