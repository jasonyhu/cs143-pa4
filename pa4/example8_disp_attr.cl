class Main {
    foo(s : String) : String {
        s
    };
    main(): IO {
        (new IO).out_string(foo("hello\n"))
    };
};