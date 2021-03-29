# tasty-checklist

This module provides the ability to run a Checklist of several
"checks" during a single test.  A "bad" check does not immediately
result in a test failure; at the end of the test (passed or failed due
to primary testing), all failed checks are reported (and any failed
checks will result in an overall test failure at the end.

This type of checking can be very useful when needing to test various
aspects of an operation that is complex to setup, has multiple
effects, or where the checks are related such that knowing about the
multiple failures makes debugging easier.

An alternative approach is to have some sort of common preparation
code and use a separate test for each item.  This module simply
provides a convenient method to collate related items under the aegis
of a single test.

This package also provides the `checkValues` function which can be
used to check a number of derived values from a single input value via
a checklist.  This can be used to independently verify a number of
record fields of a data structure or to validate related operations
performed from a single input.

See the documentation for `check` and `checkValues` for examples of
using this library.  The tests in the source package also provide
additional examples of usage.
