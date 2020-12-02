let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke

let dat = seznam(preberi_datoteko "day_2/day_2.in")

let a = "1-3 b: abwa"

let razbij vrstica = match String.split_on_char '-' vrstica with
    | [] -> []
    | prvi_int :: ostalo -> match ostalo with
        | [] -> []
        |x :: xs -> prvi_int :: String.split_on_char ' ' x

let stevilo_pojavitev niz znak = List.length(String.split_on_char znak niz) - 1

let preveri vrstica = match razbij vrstica with
    | spodnja :: zgornja :: char :: geslo :: ostalo -> (
        let spodnja_m = int_of_string spodnja in
        let zgornja_m = int_of_string zgornja in
        let znak = String.get char 0 in
        if stevilo_pojavitev geslo znak >= spodnja_m && stevilo_pojavitev geslo znak <= zgornja_m then true else false  
    )
    | _ -> false

let prestej_pravilne seznam =
    let rec pomozna stevec seznam = match seznam with
        | [] -> stevec
        | geslo :: ostala -> if preveri geslo then pomozna (stevec + 1) ostala else pomozna stevec ostala in
    pomozna 0 seznam

let na_katerih_mestih geslo spodnja zgornja znak = 
    if (geslo.[spodnja - 1] = znak && geslo.[zgornja - 1] <> znak) || (geslo.[spodnja - 1] <> znak && geslo.[zgornja - 1] = znak) then true else false


let preveri_veljavna_mesta vrstica = match razbij vrstica with
    | spodnja :: zgornja :: char :: geslo :: ostalo -> (
        let spodnja_m = int_of_string spodnja in
        let zgornja_m = int_of_string zgornja in
        let znak = char.[0] in
            na_katerih_mestih geslo spodnja_m zgornja_m znak
    )    
    | _ -> false

let prestej_veljavna seznam =
    let rec pomozna stevec seznam = match seznam with
        | [] -> stevec
        | geslo :: ostala -> if preveri_veljavna_mesta geslo then pomozna (stevec + 1) ostala else pomozna stevec ostala in
    pomozna 0 seznam


let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko "day_2/day_2.in" in
    let odgovor1 = string_of_int(prestej_pravilne dat)
    and odgovor2 = string_of_int(prestej_veljavna dat)
    in
    izpisi_datoteko "day_2/day_2_1.out" odgovor1;
    izpisi_datoteko "day_2/day_2_2.out" odgovor2;