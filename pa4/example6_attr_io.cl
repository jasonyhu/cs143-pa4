class Main {
  	var : String <- "hello\n";
    meow : String <- "meowww\n";
    main(): IO {{
        (new IO).out_string(var);
        (new IO).out_string(meow);
    }};
};