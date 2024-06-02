(*******************************************************************************
* File: snowman.cl
* Author: Drew Wadsworth
* May 2020
*
* This is a complete, playable snowman program. Uses initial user input in place
* of a random number generator. Has a list of words hardcoded.
*******************************************************************************)

class String_Util {
    str_at(index : Int, str: String) : String {
        str.substr(index, 1)
    };

    index_of(char : String, n : Int, str : String) : Int {
        let i : Int <- 0, -- parsing error of no , exits immediately <- fix
        num_seen : Int <- 0 in {
            while if i < str.length() then num_seen < n else false fi loop {
                num_seen <- num_seen + if str_at(i, str) = char then 1 else 0 fi;
                i <- i + 1;
            } pool;

            if num_seen = n then i - 1 else ~1 fi;
        }
    };

    next_index_of(char : String, start_index : Int, str : String) : Int {
        let i : Int <- start_index,
        found : Bool <- false in {
            while if i < str.length() then not str_at(i, str) = char else false fi loop {
                i <- i + 1;
            } pool;

            i;
        }
    };
};

class WordList {
    num_words : Int <- 8;
    words : String <- "extravagant,redundant,inefficient,predicate,arteriole,sasquatch,recumbent,terracing";
    delim : String <- ",";

    -- have long string of words
    -- have number of commas
    -- get number from main
    -- do some hashing to number to fit into num words
    -- get the word that corresponds to number
    -- return that word

    get_word(num : Int) : String {
        -- the below expression is a modulo / simple hash function
        let index : Int <- (num * 13 + 7) - (num * 13 + 7) / num_words * num_words,
        util : String_Util <- new String_Util in {
            index <- if index < 0 then ~index else index fi;
            let start_index : Int <- util.index_of(delim, index, words) + 1,
            end_index : Int <- util.next_index_of(delim, start_index, words) in
            words.substr(start_index, end_index - start_index);
        }
    };
};

class GuessList {
    list : String <- "";

    contains(letter : String) : Bool {
        let util : String_Util <- new String_Util in
        not util.index_of(letter, 1, list) = ~1
    };

    add(letter : String) : Bool {
        if not contains(letter) then {
            list <- list.concat(letter);
            true;
        } else false
        fi  
    };

    get() : String {
        list
    };
};

class Word {
    word : String;
    guessed : String <- "";

    init(str : String) : Object { {
        word <- str;
        let i : Int <- 0 in
        while i < str.length() loop {
            guessed <- guessed.concat("-");
            i <- i + 1;
        } pool;
    } };

    reveal(letter : String) : Int {
        let util : String_Util <- new String_Util in
        let count : Int <- 0 in
        let i : Int <- 0 in {
            while not i = word.length() loop {
                i <- util.next_index_of(letter, i, word);
                if not i = word.length() then {
                    guessed <- guessed.substr(0, i).concat(letter)
                            .concat(guessed.substr(i + 1, guessed.length() - (i + 1)));
                    count <- count + 1;
                    i <- i + 1;
                } else 0 fi;
            } pool;
            count;
        }
    };

    get() : String {
        guessed
    };

    done() : Bool {
        word = guessed
    };
};

class Main {
    begin(io : IO) : String { {
        io.out_string("Welcome to COOL snowman.\n");
        io.out_string("Please input an integer: ");

        let int : Int <- io.in_int(),
        words : WordList <- new WordList in
        words.get_word(int);
    } };

    main() : Object {
        let cont_loop : Bool <- true,
        io : IO <- new IO,
        input_word : String <- begin(io),
        word : Word <- new Word,
        guess_list : GuessList <- new GuessList,
        num_guesses : Int <- 6 in {
            word.init(input_word);

            while if not num_guesses <= 0 then not word.done() else false fi loop {
                io.out_string("Word: ".concat(word.get()).concat("\n"));
                io.out_string("So far, you have guessed: ".concat(guess_list.get()).concat("\n"));
                io.out_string("You have ");
                io.out_int(num_guesses);
                io.out_string(" guesses remaining.\n");
                io.out_string("Guess a letter: ");
                
                let letter : String <- io.in_string() in
                if letter.length() = 1 then
                    if guess_list.add(letter) then
                        let places : Int <- word.reveal(letter) in
                        if not places = 0 then {
                            io.out_string("Good guess! ".concat(letter).concat(" is in the word "));
                            io.out_int(places);
                            io.out_string(" times.\n");
                        } else {
                            io.out_string("Bummer. ".concat(letter).concat(" is not in the word.\n"));
                            num_guesses <- num_guesses - 1;
                        } fi
                    else
                        io.out_string("You have already guessed ".concat(letter).concat("\n"))
                    fi
                else
                    io.out_string("Please guess a single letter.\n")
                fi;

                io.out_string("\n");
            } pool;

            if num_guesses = 0 then
                io.out_string("Bummeroonie, I'm so sorry. You have run out of guesses. ")
            else
                io.out_string("Congrats, you win! ")
            fi;

            io.out_string("The word was ".concat(input_word).concat(".\n"));
        }
    };
};
