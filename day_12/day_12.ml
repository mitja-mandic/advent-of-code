open String
open List


let day = "12"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let list_parov ime_datoteke = 
    let vsebina = preberi_datoteko ime_datoteke in
    let seznam = String.split_on_char '\n' vsebina in
    List.map (fun x -> (x.[0], int_of_string(String.sub x 1 ((String.length x) - 1)))) seznam

let manhattan seznam_ukazov = 
    let rec aux x y kot = function
        | [] -> (Int.abs x) + (Int.abs y)
        | (ukaz, vrednost) :: ostalo -> 
            match ukaz with
                | 'N' -> aux x (y + vrednost) kot ostalo
                | 'S' -> aux x (y - vrednost) kot ostalo
                | 'E' -> aux (x + vrednost) y kot ostalo
                | 'W' -> aux (x - vrednost) y kot ostalo
                | 'L' -> 
                    let zasuk_l = kot + vrednost in 
                    if zasuk_l < 0 then aux x y (360 + (zasuk_l mod 360)) ostalo else aux x y (zasuk_l mod 360) ostalo
                | 'R' ->
                    let zasuk_d = kot - vrednost in
                    if zasuk_d < 0 then aux x y (360 + (zasuk_d mod 360)) ostalo else aux x y (zasuk_d mod 360) ostalo
                | 'F' -> match kot with
                        | 0 -> aux (x+vrednost) y kot ostalo
                        | 90 -> aux x (y+vrednost) kot ostalo 
                        | 180 -> aux(x-vrednost) y kot ostalo
                        | 270 -> aux x (y-vrednost) kot ostalo 
                | _-> failwith "no go amigo" in
    aux 0 0 0 seznam_ukazov
    
let  naloga1 datoteka = datoteka |> list_parov |> manhattan |> string_of_int

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1 input
    
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
   