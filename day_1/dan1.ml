let int_list vsebina_datoteke = 
    let string_sez = String.split_on_char '\n' vsebina_datoteke in
        List.map int_of_string string_sez

let rec vsota_2020 seznam = 
    match seznam with
    | x :: y :: xs -> if x+y == 2020 then x * y else vsota_2020 (x :: xs)
    | _ -> 0

let rec zares_2020 seznam = 
    match seznam with
    | [] -> 0
    | x :: xs -> if vsota_2020 seznam == 0 then zares_2020 xs else vsota_2020(seznam)

let naloga1 vsebina_datoteke =
    match zares_2020 (int_list vsebina_datoteke) with
    | 0 -> "ne gre"
    | _ -> string_of_int(zares_2020 (int_list vsebina_datoteke))


(*drugi del *)
let rec vsota_treh seznam =
    match seznam with
    | x :: y :: z :: xs -> if x + y + z == 2020 then x * y * z else vsota_treh (x :: y :: xs)
    | _ -> 0

let rec vsota_dveh seznam = 
    match seznam with
    | x :: y :: xs -> if vsota_treh seznam != 0 then vsota_treh seznam else vsota_dveh (x :: xs)
    | _ -> 0

let rec vsota seznam = 
    match seznam with
    | x :: xs -> if vsota_dveh seznam != 0 then vsota_dveh seznam else vsota xs
    | _ -> 0


let naloga2 vsebina_datoteke =
    match vsota (int_list vsebina_datoteke) with
    | 0 -> "ne gre"
    | _ -> string_of_int(vsota (int_list vsebina_datoteke))

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
    let vsebina_datoteke = preberi_datoteko "day_1/day_1.in" in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko "day_1/day_1_1.out" odgovor1;
    izpisi_datoteko "day_1/day_1_2.out" odgovor2;