# How to contribute

## Adding new features

A new feature should be:

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

## Integration and unit tests

TODO: put Kerem's description here.

## Coding style

We
use
[Johan Tibell](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md) style
guide because:

- It is a lightweight style guide.
- It is quite permissive, but also sensible.
- It includes useful guidelines for avoiding unnecessary lazy constructor
  fields.

