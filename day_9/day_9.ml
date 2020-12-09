let day = "9"

let input = "day_" ^ day ^"/day_" ^day^".in"

let preberi_datoteko ime_datoteke =
    let chan = open_in ime_datoteke in
    let vsebina = really_input_string chan (in_channel_length chan) in
    close_in chan;
    vsebina

let int_list vsebina_datoteke = 
    let string_sez = String.split_on_char '\n' vsebina_datoteke in
        List.map int_of_string string_sez

let preveri_seznam seznam vrednost = 
    let rec s_prvim acc = function
        | x :: y :: ostalo -> s_prvim ((x+y)::acc) (x::ostalo) 
        | _ -> acc in
    let rec vsote acc seznam = match seznam with
        | x::xs -> vsote ((s_prvim [] seznam)@acc) xs
        | _-> acc in
    if List.mem vrednost (vsote [] seznam) then 0 else vrednost

let rec podseznam i j seznam = 
    if i > j then []
    else (List.nth seznam i) :: (podseznam (i+1) j seznam)

let najdi_vrednost seznam = 
    let rec aux seznam pozicija =
        if pozicija >= List.length seznam then failwith "no go" else

        let podseznam = podseznam (pozicija-25) (pozicija-1) seznam in
        
        let vsota = preveri_seznam podseznam (List.nth seznam pozicija) in
        
        if vsota = 0 then aux seznam (pozicija+1) else vsota in

    aux seznam 25

let naloga1 = input |> preberi_datoteko |> int_list |> najdi_vrednost |> string_of_int



let rec sestevaj seznam stevilka =
    let rec aux sez acc = 
        if List.fold_left (+) 0 acc = stevilka then acc else
        match sez with
        | x :: xs -> aux xs (x::acc)
        | [] -> sestevaj (List.tl seznam) stevilka in
    aux seznam []

let rec zadnji = function
    | x::[] -> x
    | x :: xs -> zadnji xs
    | _ -> failwith "ojoj"


let naloga2 ime_datoteke = 
    let seznam = ime_datoteke |> preberi_datoteko |> int_list in
    let stevilka = int_of_string naloga1 in
    let urejen = List.sort compare (sestevaj seznam stevilka) in
    let vrednost = (List.hd urejen) + zadnji urejen in
    string_of_int vrednost

let _ =

    let izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    
    let odgovor1 = naloga1
    and odgovor2 = naloga2 input
    in
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_1.out") odgovor1;
    izpisi_datoteko ("day_" ^ day ^ "/day_" ^ day ^ "_2.out")  odgovor2;