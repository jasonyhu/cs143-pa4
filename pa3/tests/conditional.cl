-- Tests conditional

class Main inherits IO {

    a : Bool <- true;

    main() : SELF_TYPE {
        {
            if a 
            then out_string("a is true")
            else out_string("a is true")
            fi;
        }
    };
};