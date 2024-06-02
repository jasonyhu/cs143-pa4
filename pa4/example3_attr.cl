class Main {
    a: IO <- new IO;
    b: Mom;
    main():IO {{
        a.out_int(b.printB());
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