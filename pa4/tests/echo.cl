class Main inherits IO {
   main() : Object {
      { (let x : String <- in_string() in
	{ out_string(x); out_string("\n"); }
      ); }
   };
};

