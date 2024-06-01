class Main inherits IO {
    pal(s : String) : Bool {
        {
        out_int(s.length());
        if s.length() = 0
        then  true
        else if s.length() = 1
        then true
        else if s.substr(0, 1) = s.substr(s.length() - 1, 1)
        then pal(s.substr(1, s.length() -2))
        else false
        fi fi fi;
        }
    };

    a(s : String) : Int {
        {
            out_string(s.substr(0, 1));
            out_string("\n");
            out_string(s.substr(s.length() - 1, 1));
            out_string("\n");
            if s.substr(0, 1) = s.substr(s.length() - 1, 1)
            then out_string("s.substr(0, 1) = s.substr(s.length() - 1, 1)")
            else out_string("s.substr(0, 1) != s.substr(s.length() - 1, 1)")
            fi;
            out_string("\n");
            if s.length() = 0
            then 1
            else if s.length() = 1
            then 1
            else if s.substr(0, 1) = s.substr(s.length() - 1, 1)
            then a(s.substr(1, s.length() -2))
            else 0
            fi fi fi;
        }
    };

    i : Int;

    main() : SELF_TYPE {
	    {
            i <- ~1;
            out_int(a(in_string()));
            out_string("\n");
            out_string("enter a string\n");
            if pal(in_string())
            then out_string("that was a palindrome\n")
            else out_string("that was not a palindrome\n")
            fi;
	    }
    };
};