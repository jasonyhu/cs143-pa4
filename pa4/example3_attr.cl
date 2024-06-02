class Main {
    a: IO <- new IO;
    b: Mom <- new Mom;
    main():IO {{
        a.out_int(b.printB());
        a.out_int(b.printA());
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