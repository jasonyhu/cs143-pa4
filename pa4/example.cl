class A {
  a : Int <- 5;
  b : String;
  foo() : A {
    new SELF_TYPE
  };
};

class Main inherits A {
  c: Int;
  main(): Object {
    foo()
  };
};
