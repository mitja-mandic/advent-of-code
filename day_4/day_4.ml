open Printf
open String

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke

let dat = seznam(preberi_datoteko "day_4/day_4.in")

(* razbije na sezname po osebi, ampak z gnezdenimi seznami *)
let rec razbij seznam prazen = 
    let rec dodaj seznam gradimo = match seznam with
        | [] -> [gradimo]
        | x :: xs -> if x <> "" then dodaj xs ((String.split_on_char ' 'x)::gradimo) else razbij xs prazen @ [gradimo] in
    dodaj seznam []
    
let razbit = razbij dat []

(*Pomožna funkcija. Vrne en seznam z vsemi podatki posameznika*)
let rec en_seznam prazen = function
    | [] -> prazen
    | x :: xs -> 
        let rec pomozna podatek aux = 
                match podatek with
                | [] -> en_seznam (aux @ prazen) xs
                | neki :: ostalo -> pomozna ostalo (neki::aux) in
    pomozna x []

(*Vrne seznam seznamov podatkov za vse *)
let rec poenoti seznam gradim = match seznam with
    | [] -> gradim
    | x :: xs -> poenoti xs [en_seznam [] x] @ gradim

let preveri_vrstico seznam = 
    if List.length seznam = 8 then true else
        if List.length seznam = 7  && not(List.mem "cid" (en_seznam [] (List.map(String.split_on_char ':') seznam))) then true else false

let prestej_legalne seznam = 
    let rec pomozna stevec seznam = match seznam with
        | [] -> stevec
        | x :: ostalo -> if preveri_vrstico x then pomozna (1 + stevec) ostalo else pomozna stevec ostalo in
    pomozna 0 seznam


(* DRUGA NALOGA*)

let po_dvopicju seznam = List.map(String.split_on_char ':') seznam

let niz_v_par niz = match String.split_on_char ':' niz with
    | a::b::[] -> (a,b)
    | _ -> failwith "ne bo šlo"

let vrstica_v_pare seznam = 
    let rec aux seznam acc = match seznam with
        | [] -> acc
        | x :: y -> aux y ((niz_v_par x )::acc) in
    aux seznam []


let preveri_hgt niz = 
    if String.length niz = 4 && (String.sub niz 2 2) = "in" then 
        int_of_string (String.sub niz 0 2) <= 76 && int_of_string (String.sub niz 0 2) >= 59 
    else if String.length niz = 5 && (String.sub niz 3 2) = "cm" then
        int_of_string (String.sub niz 0 3) <= 193 && int_of_string (String.sub niz 0 3) >= 150 else false


let sez = ['#';'0';'1';'2';'3';'4';'5';'6';'7';'8';'9';'a';'b';'c';'d';'e';'f']

(*vir kode https://reasonml.chat/t/iterate-over-a-string-pattern-match-on-a-string/1317 *)
let seznam_znakov string = string |> String.to_seq |> List.of_seq

let a_so_notr niz testni= 
    let rec pomozna seznam testni= match seznam with
        | [] -> true
        | x :: xs -> if List.mem x testni then pomozna xs testni else false in
    pomozna (seznam_znakov niz) testni

let preveri_hcl niz = 
    if String.length niz = 7 && niz.[0] = '#' then a_so_notr niz sez else false

let preveri_tuple par = match par with
    | (a, b) when a = "byr" -> 1920 <= int_of_string b && int_of_string b <= 2002
    | (a, b) when a = "iyr" -> 2010 <= int_of_string b && int_of_string b <= 2020
    | (a, b) when a = "eyr" -> 2020 <= int_of_string b && int_of_string b <= 2030
    | (a, b) when a = "hgt" -> preveri_hgt b
    | (a, b) when a = "hcl" -> preveri_hcl b
    | (a, b) when a = "ecl" -> List.mem b ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
    | (a, b) when a = "pid" -> String.length b = 9
    | _ -> true

let rec prestej_vrstico_tuple = function 
    | [] -> true
    | x :: xs -> if preveri_tuple x then prestej_vrstico_tuple xs else false

let rec prestej seznam stevec = match seznam with
    | [] -> stevec
    | x::xs -> if preveri_vrstico x && prestej_vrstico_tuple (vrstica_v_pare x) then prestej xs (stevec+1) else prestej xs stevec


let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = string_of_int(prestej_legalne (poenoti razbit []))
    and odgovor2 = string_of_int(prestej (poenoti razbit []) 0)
    in
    izpisi_datoteko "day_4/day_4_1.out" odgovor1;
    izpisi_datoteko "day_4/day_4_2.out" odgovor2;