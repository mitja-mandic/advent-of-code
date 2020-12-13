let day = "10"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let int_list vsebina_datoteke = 
    let string_sez = String.split_on_char '\n' vsebina_datoteke in
        List.map int_of_string string_sez

let uredi seznam = List.sort compare seznam

let prestej_razlike seznam =
    let rec aux seznam ena tri prvi = match seznam with
        | x :: y :: xs -> if y - x = 1 then aux (y::xs) (ena+1) tri x
                        else if y - x = 3 then aux (y::xs) ena (tri+1) x else failwith "ojoj"
        | x :: [] -> if x - prvi = 1 then aux [] (ena+1) tri x else aux [] ena (tri+1) x 
        | [] ->  (tri+1) * ena in
    aux seznam 0 0 (List.hd seznam)

let naloga1 datoteka = datoteka |> preberi_datoteko |> int_list |> uredi |> prestej_razlike |> string_of_int

let _ =
    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1 input
    
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
    