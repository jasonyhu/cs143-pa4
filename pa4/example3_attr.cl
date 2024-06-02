class Main {
    a: IO <- new IO;
    b: Mom;
    main():IO {{
        (new IO).out_int((new Mom).printB());
        (new IO).out_int((new Mom).printA());
    }
    };
};

class Parent {
    a:Int <- 5;
};

class Mom inherits Parent {
    b:Int <- 1+2;
    c:Mom;
    printB():Int { b };
    printA():Int { a };
};