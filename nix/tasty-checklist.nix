{ mkDerivation, base, containers, doctest, exceptions, HUnit, lib
, parameterized-utils, tasty, tasty-expected-failure, tasty-hunit
, text
}:
mkDerivation {
  pname = "tasty-checklist";
  version = "1.0.10.0";
  src = ./..;
  libraryHaskellDepends = [
    base containers exceptions parameterized-utils text
  ];
  testHaskellDepends = [
    base doctest HUnit parameterized-utils tasty tasty-expected-failure
    tasty-hunit text
  ];
  description = "Check multiple items during a tasty test";
  license = lib.licenses.isc;
}
