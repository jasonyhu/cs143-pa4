class Main {
    main():Int {1};
};

class Parent {
    a:Int <- 5;
};

class Mom inherits Parent {
    b:Int <- 1+2;
    c:Mom;
};