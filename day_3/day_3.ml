let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let testni = ["..##.......";"#...#...#..";".#....#..#.";"..#.#...#.#";".#...##..#.";"..#.##.....";".#.#.#....#";".#........#";"#.##...#...";"#...##....#";".#..#...#.#"]

let seznam vsebina_datoteke = String.split_on_char '\n' vsebina_datoteke


let dat = seznam(preberi_datoteko "day_3/day_3.in")



let veljaven indeks premik vrstica = if indeks + premik >= String.length vrstica then (false, indeks + premik - String.length vrstica) else (true, indeks + premik)

let a_je_drevo indeks niz = if niz.[indeks] = '#' then 1 else 0

let a = "#...#..........#..#............"

let rec prestej desno pozicija seznam = match seznam with
    | [] -> 0
    | x :: [] -> if pozicija + desno < String.length x then a_je_drevo (pozicija + desno) x else 0
    | x :: y :: ostali -> 
        match veljaven pozicija desno x with
        |(_, premik) -> a_je_drevo premik y + prestej desno premik (y::ostali)


let rec dva_dol desno pozicija seznam = match seznam with
    | [] -> 0
    | x :: preskoci :: y :: [] -> if pozicija + desno < String.length y then a_je_drevo (pozicija + desno) y else 0
    | x :: preskoci :: y :: ostali -> 
        match veljaven pozicija desno x with
            | (_, premik) -> a_je_drevo premik y + dva_dol desno premik (y::ostali);
    | _ -> 0

let naloga2 =
    let ena = prestej 1 0 dat in
    let tri = prestej 3 0 dat in
    let pet = prestej 5 0 dat in
    let sedem = prestej 7 0 dat in
    let dol = dva_dol 1 0 dat in
    ena * tri * pet * sedem * dol

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = string_of_int(prestej 3 0 dat)
    and odgovor2 = string_of_int(naloga2)
    in
    izpisi_datoteko "day_3/day_3_1.out" odgovor1;
    izpisi_datoteko "day_3/day_3_2.out" odgovor2;