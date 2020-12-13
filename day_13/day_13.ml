let day = "13"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let int_list vsebina_datoteke = 
    let string_sez = String.split_on_char '\n' vsebina_datoteke in
    let nov_sez = List.map (String.split_on_char ',') string_sez |> List.map (List.filter (fun x -> x <> "x")) in
    let urejen = List.concat nov_sez |> List.map int_of_string in
    (List.hd urejen, List.tl urejen)



let rec sez_vecji_od vrednost seznam =
    let rec aux sez razlike = match sez with
        | x :: xs -> 
            let spodnja = (vrednost - (vrednost mod x))/x in
            let razlika = (spodnja + 1)*x - vrednost in
            aux xs ([(x,razlika)] @ razlike) 
        | [] -> List.sort (fun (x,y) (u,v) -> compare y v) razlike in    
    let (x,y) = List.hd (aux seznam []) in
    x * y


let naloga1 datoteka = 
    let (vrednost,sez) = datoteka |> preberi_datoteko |> int_list in
    (sez_vecji_od vrednost sez) |> string_of_int

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1 input
    
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
    