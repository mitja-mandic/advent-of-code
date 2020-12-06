let day = "6"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke
let dat = seznam(preberi_datoteko input)

let razbij seznam = 
    let rec razbij_seznam seznam prazen = 
        let rec dodaj seznam gradimo = match seznam with
            | [] -> [gradimo]
            | x :: xs -> if x <> "" then dodaj xs ((String.split_on_char ' ' x)::gradimo) else razbij_seznam xs prazen @ [gradimo] in
        dodaj seznam [] in
    razbij_seznam seznam []

let en_seznam seznam = 
    let rec pomozna1 prazen = function
        | [] -> prazen
        | x :: xs -> 
            let rec pomozna podatek aux = 
                    match podatek with
                    | [] -> pomozna1 (aux @ prazen) xs
                    | neki :: ostalo -> pomozna ostalo (neki::aux) in
        pomozna x [] in 
    pomozna1 [] seznam

let poenoti seznam = 
    let rec pomozna seznam gradim = match seznam with
        | [] -> gradim
        | x :: xs -> pomozna xs [en_seznam x] @ gradim in
    pomozna seznam []

(*vir kode https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317 *)
let seznam_znakov string = string |> String.to_seq |> List.of_seq

(*vir kode https://rosettacode.org/wiki/Remove_duplicate_elements#OCaml *)
let odstrani_dvojnike seznam = List.sort_uniq compare seznam

let prestej_enega seznam = 
    let rec pomozna seznam delovni = match seznam with
        | [] -> List.length(odstrani_dvojnike delovni)
        | x :: ostali -> pomozna ostali ((seznam_znakov x) @ delovni) in
    pomozna seznam []

let prestej seznam = 
    let rec pomozna seznam stevec = match seznam with
        | [] -> stevec
        | x :: xs -> pomozna xs (stevec + prestej_enega x) in
    pomozna seznam 0