let day = "6"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke

 (* koda za razbijanje seznama je prirejena po moji kodi iz dneva 4 *)
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

let dat = input |> preberi_datoteko |> seznam


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

let naloga1 = input |> preberi_datoteko |> seznam |> razbij |> poenoti |> prestej |> string_of_int

(*https://discuss.ocaml.org/t/how-to-sort-a-string-in-ocaml/4904/3*)
let sort s = 
  String.to_seq s |> List.of_seq |> List.sort Char.compare |> List.to_seq |> String.of_seq

let rec uredi_nize = function
    | [] -> []
    | x :: xs -> (sort x) :: (uredi_nize xs)

let v_vseh seznami char = if  List.length (odstrani_dvojnike (List.map (List.mem char) (List.map seznam_znakov seznami))) > 0 then List.hd(odstrani_dvojnike (List.map (List.mem char) (List.map seznam_znakov seznami)))
    else true


let prestej_vrstico seznam chars =
    let rec aux seznam znaki count = match znaki with
        | [] -> count
        | x :: xs -> if v_vseh seznam x then aux seznam xs (count+1) else aux seznam xs count in
    aux seznam chars 0

let prestej_dvojne seznam = 
    let rec pomozna stevec seznam = match seznam with
        | [] -> stevec
        | x :: xs -> match x with
            | znaki :: ostalo -> pomozna (stevec + prestej_vrstico ostalo (seznam_znakov znaki)) xs
            | _ -> failwith "tema" in
    pomozna 0 seznam
        
let naloga2 = input |> preberi_datoteko |> seznam |> razbij |> poenoti |> prestej_dvojne |> string_of_int

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1
    and odgovor2 = naloga2
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^"_1.out") odgovor1;
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^"_2.out")  odgovor2;