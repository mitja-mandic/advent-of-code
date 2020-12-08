open List
let day = "8"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke

let pari seznam =
    let rec aux acc seznam = match seznam with
        | [] -> acc
        | x :: xs -> 
            let razbit = String.split_on_char ' ' x in
            match razbit with
                | ukaz :: vrednost :: [] -> aux (acc@[(ukaz, int_of_string vrednost)]) xs
                | _ -> failwith "no go" in
    aux [] seznam



let rec nti_element seznam indeks = match seznam with
    | [] -> failwith "nekaj ne štima"
    | x :: xs -> if indeks = 0 then x else nti_element xs (indeks-1)

let sestej seznam = 
    let rec pomozna seznam indeksi akumulator pozicija =
            if List.mem pozicija indeksi then akumulator else
                let trenutni = nti_element seznam pozicija in
                match trenutni with
                    | (ukaz, akcija) -> if ukaz = "nop" then pomozna seznam (pozicija::indeksi) akumulator (pozicija + 1) else
                                        if ukaz = "acc" then pomozna seznam (pozicija::indeksi) (akumulator+akcija) (pozicija + 1) else
                                        if ukaz = "jmp" then pomozna seznam (pozicija::indeksi) akumulator (pozicija + akcija) else failwith "no go amigo" in

    pomozna seznam [] 0 0

let naloga1 = input |> preberi_datoteko |> seznam |> pari |> sestej |> string_of_int

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1
    (*and odgovor2 = naloga2*)
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
    (*izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_2.out")  odgovor2;*)