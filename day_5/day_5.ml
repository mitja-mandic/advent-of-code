let day = "5"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke


(* dobi razbito vrstico *)
let izracunaj_pozicijo pozicija znak =
    let (min, max) = pozicija in
    let sredina = (max - min)/2 in
    if (max - min) > 1 then
        match znak with
            | 'F' | 'L' -> (min, min + sredina)
            | 'B' | 'R'-> (max - sredina, max)
            | _ -> failwith "invalid"
    else
        match znak with
            | 'F' | 'L' -> (min, min)
            | 'B' | 'R' -> (max , max)
            | _ -> failwith "invalid"

(*vir kode https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317 *)
let seznam_znakov string = string |> String.to_seq |> List.of_seq

let vrsta seznam =
    let rec pomozna pozicija seznam = 
        match seznam with
            | [] -> pozicija
            | x :: xs-> 
                if x = 'B' || x = 'F' then
                    pomozna (izracunaj_pozicijo pozicija x) xs
                else pozicija in
    

    pomozna (0,127) seznam

let stolpec seznam =
    let rec pomozna pozicija seznam = 
        match seznam with
            | [] -> pozicija
            | x :: xs -> 
    
                if x = 'L' || x = 'R' then
                    pomozna (izracunaj_pozicijo pozicija x) xs
                else pozicija in

    pomozna (0,7) seznam

let id_vrstice niz = 
    let vrstica = String.sub niz 0 7 in
    let stolp = String.sub niz 7 3  in
    let (vrsta,_) = vrstica |> seznam_znakov |> vrsta in
    let (stolpec,_) = stolp |> seznam_znakov |> stolpec in
    8 * vrsta + stolpec


let najdi_max = function
    | [] -> failwith "empty list"
    | x::xs -> List.fold_left max x xs

let seznam_id seznam = 
    let rec aux seznam acc = 
        match seznam with
            | [] -> acc
            | x :: xs -> aux xs ((id_vrstice x)::acc) in
    aux seznam []

let naloga1 = input |> preberi_datoteko |> seznam |> seznam_id |> najdi_max |> string_of_int

let najdi_zaporedne seznam =
    let rec pomozna stevec seznam = 
        if List.mem (stevec - 1) seznam && List.mem (stevec + 1) seznam && not(List.mem stevec seznam) then 
            if stevec > 100 then stevec else pomozna (stevec+1) seznam 
        else pomozna (stevec+1) seznam in
    pomozna 0 seznam


let naloga2 = input |> preberi_datoteko |> seznam |> seznam_id |> najdi_zaporedne |> string_of_int

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1
    and odgovor2 = naloga2
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_2.out")  odgovor2;