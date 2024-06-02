class Main {
   b : IO <- new IO; 
   c: Bool <- true;  
    test_nestedlets(val1:Int, val2:Int, val3:Int, valx : Int): Bool {
      {
         b.out_int(1);
         b.out_int(2);
         let val1:Int <- 1 + 2,
             val2:Int <- 1 - 3 + val3
            in 
            {
               b.out_int(1);
               b.out_int(2);
               b.out_int(val1);
               b.out_int(val2);
               true;
            };
      }
   };
   main() : Object {
      {
         if not c then
            test_nestedlets(5, 4, 3, 2)
         else 
            test_nestedlets(9, 8, 7, 6)
         fi;
      }
   };
};

