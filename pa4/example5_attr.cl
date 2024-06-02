class Main {
  	var : String <- "hello\n";
    main(): IO {
        (new IO).out_string(var)
    };
};