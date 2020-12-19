let day = "15"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let v_seznam vsebina_datoteke = 
    (preberi_datoteko vsebina_datoteke) |> String.split_on_char ',' |> List.map int_of_string

let rec najdi seznam stevilo =
    match seznam with
    | [] -> 0
    | x :: xs -> if x = stevilo then 0 
    else 1 + najdi xs stevilo

let rec prestej_pojavitve seznam stevilo = 
    match seznam with
    | [] -> 0
    | x :: xs -> if x=stevilo then 1 + prestej_pojavitve xs stevilo else prestej_pojavitve xs stevilo

let najdi_naslednje seznam prejsnje =
    let prva = prestej_pojavitve seznam prejsnje in
    if prva <= 1 then 0 else 
        let rep = seznam |> List.rev |> List.tl in
        let zadnji = List.length rep + 1 in
        let predzadnji = List.length seznam - 1 - najdi rep prejsnje in
        zadnji - predzadnji

let stevilo seznam ponovitve=
    let zacetek = List.length seznam + 1 in
    let delovni = ref seznam in
    for  i = zacetek to ponovitve do
        let zadnji_element = List.hd(List.rev !delovni) in 
        let naslednji = najdi_naslednje !delovni zadnji_element in
        delovni := !delovni @ [naslednji]
    done;
    !delovni |> List.rev |> List.hd

let naloga1 datoteka = 
    let sez = datoteka |> v_seznam in
    stevilo sez 2020 |> string_of_int

(*let naloga2 datoteka = 
    let sez = datoteka |> v_seznam in
    stevilo sez 30000000 |> string_of_int*)

let _ =
    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let odgovor1 = naloga1 input
    (*and odgovor2 = naloga2 input*)
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
    (*izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor2;*)

