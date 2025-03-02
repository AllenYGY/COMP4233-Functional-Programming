#use "Lab_2_Exercise.ml";;

let m = 2;;

let str =string_of_int m;;
for i= 0 to String.length(str)-1 do( 
   if  (int_of_char (str.[i]) - int_of_char '0') < 5 && (int_of_char (str.[i]) - int_of_char '0') >=0 
    then ( print_char (str.[i]); print_newline());  
   ) 
done;;