class Main inherits IO {
    s : String <- "abba";
    main() : SELF_TYPE {
        {
            case (new Object) of
                y : String => {out_string(y);};
                y : Object => {out_string("not a string");};
            esac;
	    }
    };
};