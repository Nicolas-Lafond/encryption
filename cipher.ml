(* a datatype for letter 
 * the int value is the order of the letter in the alphabet *)
type letter = 
    | Upper_Case of int
    | Lower_Case of int
    | Not_A_Letter 

let char_to_letter c =
    let char_value = int_of_char c in
    if char_value <= int_of_char 'Z' && char_value >= int_of_char 'A' 
        then Upper_Case (char_value - int_of_char 'A')
    else if char_value <= int_of_char 'z' && char_value >= int_of_char 'a' 
        then Lower_Case (char_value - int_of_char 'a')
    else Not_A_Letter

let letter_to_char l = match l with
    | Upper_Case o -> char_of_int (o + int_of_char 'A')
    | Lower_Case o -> char_of_int (o + int_of_char 'a')
    | Not_A_Letter -> ' '

let string_to_list str = 
    let rec aux s l i len = 
        if i < len 
        then aux s (l @ [s.[i]]) (i + 1) len
        else l
    in aux str [] 0 (String.length str)

let _right_shift letter number = match letter with
    | Upper_Case o 
        -> if o + number > 25 then Upper_Case (o + number - 26)
                     else Upper_Case (o + number)
    | Lower_Case o
        -> if o + number > 25 then Lower_Case (o + number - 26)
                     else Lower_Case (o + number)
    | Not_A_Letter -> Not_A_Letter

let right_shift number character = 
    let number = number mod 26 in
    let letter = char_to_letter character in
    match letter with 
        | Not_A_Letter -> character
        | _ -> letter_to_char (_right_shift letter number)

let _simple_substitution k c =
    let letter = char_to_letter c in
    match letter with
        | Upper_Case o -> k.[o]
        | Lower_Case o -> k.[o]
        | _ -> c
    
(* Encryption functions starts here *)
let cesar_cipher message shift_val = 
    String.map (right_shift shift_val) message

let simple_substitution message key =
    String.map (_simple_substitution key) message
