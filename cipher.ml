open Core.Std

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

(* return a list of char from a string *)
let string_to_list str = 
    let rec aux s l i len = 
        if i < len 
        then aux s (l @ [s.[i]]) (i + 1) len
        else l
    in aux str [] 0 (String.length str)

let rec letter_list_to_string letters =
    let chars = List.map letter_to_char letters in
    let charss = List.map Char.to_string chars in
    List.fold_left (^) "" charss

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

let _vigenere_substitution k c =
    let letter_k = char_to_letter k in
    let letter_c = char_to_letter c in
    match (letter_k, letter_c) with
        | (Upper_Case o_k, Upper_Case o_c) -> Upper_Case (o_k + o_c)
        | (Upper_Case o_k, Lower_Case o_c) -> Upper_Case (o_k + o_c)
        | (Lower_Case o_k, Lower_Case o_c) -> Lower_Case (o_k + o_c)
        | (Lower_Case o_k, Upper_Case o_c) -> Lower_Case (o_k + o_c)
        | _ -> c

(* Create a list of length len containing all shift size for
 * vigenere cipher *)
let _vigenere_shift_list key len =
    let key_length = String.length key in
    let rec aux key i key_ind len res_list =
        let key_ind = if key_ind < key_length then key_ind else 0 in
        if i < len 
        then match key.[ind] with
            | Upper_Case o | Lower_Case o 
                -> aux key (i+1) (key_ind+1) len (res_list @ o)
            | _ -> aux key (i+1) (key_ind+1) len (res_list @ 0)
        else res_list
    in aux key 0 0 len []

(* take a list of integer and a list of character and return a new list of all
 * letters shifted by the number *)
let _vigenere_cipher shift_list message_list =
    let combine_list = List.combine shift_list message_list in
    let aux (s, c) = _right_shift (char_to_letter c) s in
    List.map aux combine_list
    
(* Encryption functions starts here *)
let cesar_cipher message shift_val = 
    String.map (right_shift shift_val) message

let simple_substitution message key =
    String.map (_simple_substitution key) message

let vigenere_cipher message key =
    let len = String.length message in
    let shift_list = _vigenere_shift_list key len in
    let letter_list = _vigenere_cipher shift_list (string_to_list message) in
    letter_list_to_string letter_list
    
