
(*  Example cool program testing as many aspects of the code generator
    as possible.
 *)

class Main {
  type_name():String { "Main" };
  main():IO { (new IO).out_string(type_name()) };
};

