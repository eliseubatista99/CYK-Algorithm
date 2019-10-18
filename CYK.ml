open Printf;;
open Scanf;;
open List;;

let nothing = ref 0


let explode s =
  let rec exp i l =
  match i with
   _ when i < 0 -> l
  | _ -> exp (i-1) (s.[i]::l)
        in exp (String.length s - 1) []

let normalize c = 
  fold_left 
    (fun res x -> 
      if (mem x res) 
      then res 
      else (x::res)) 
    [] c 

(* --------------------------------------------------------------------------------------------------------------- *)

let rec divide_cabeca cabeca limite contador acc =
  if contador = limite then (List.rev acc,cabeca)
  else
  match cabeca with
  | [] -> (acc,cabeca)
  | h::t -> divide_cabeca t limite (contador+1) (h::acc)


let rec iterar_mistura mistura1 mistura2 regras acc contador =
  if contador >= List.length mistura2 then acc
  else
  let get_caracter = ref [] in
  for i = 0 to ((List.length regras)-1) do 
    let primeiro,segundo = (List.nth regras i) in
    if (explode (List.hd segundo)) = [(List.nth mistura1 1);(List.nth mistura1 0)]
    then get_caracter := (primeiro::(!get_caracter)) else
    nothing := ((!nothing)+1)
  done;
  let menos_dois = List.tl (List.tl mistura1) in
  if (!get_caracter) = [] 
  then 
  iterar_mistura menos_dois mistura2 regras acc (contador+2)
  else iterar_mistura menos_dois mistura2 regras ((!get_caracter)@acc) (contador+2)

let make_maquina lista_palavras lista_numeros regras =
    let maquina = ref [] in
    let nova_lista_numeros = ref lista_numeros in
        for j=0 to ((List.length lista_palavras)-1) do
            let headzinha = List.nth lista_palavras j in
            let lista_numeros_aux = ref (!nova_lista_numeros) in
            let maquina_aux = ref (!maquina) in
                for i=0 to ((List.length headzinha)-1) do
                    let cabeca_numeros = List.hd (!lista_numeros_aux) in
                    let cabeca_da_cabeca = List.nth headzinha i in
                    let tuplante = ref [] in
                    for k=1 to ((List.length cabeca_da_cabeca)-1) do
                        let n,m = divide_cabeca cabeca_da_cabeca k 0 [] in
                        tuplante := ((n,m)::(!tuplante))
                    done;
                    if (!tuplante) = []  then
                        let valor = ref [] in
                                for z=0 to ((List.length regras)-1) do
                                    let a,b = (List.nth regras z) in
                                    if (List.mem (String.make 1 (List.hd cabeca_da_cabeca)) b) then 
                                    valor := (a::(!valor))
                                    else
                                    nothing := ((!nothing)+1)
                                done;
                        if (!valor) = [] then
                        (
                        lista_numeros_aux := (List.tl (!lista_numeros_aux));
                        maquina_aux := ([cabeca_da_cabeca,cabeca_numeros,[],(!tuplante)]@(!maquina_aux))
                        )
                        else
                        (
                        lista_numeros_aux := (List.tl (!lista_numeros_aux));
                        maquina_aux := ([cabeca_da_cabeca,cabeca_numeros,(List.rev (!valor)),(!tuplante)]@(!maquina_aux))
                        )
                    else
                        let get_informacao = ref [] in
                          for r=0 to ((List.length (!tuplante))-1) do
                              let n,m = (List.nth (!tuplante) r) in
                                  let retorno1 = ref [] in
                                  for s=0 to ((List.length (!maquina_aux))-1) do
                                  let a,b,c,d = (List.nth (!maquina_aux) s) in
                                  if a = n then retorno1 := c
                                  done;
                                  let retorno2 = ref [] in
                                  for s=0 to ((List.length (!maquina_aux))-1) do
                                  let a,b,c,d = (List.nth (!maquina_aux) s) in
                                  if a = m then retorno2 := c
                                  done;
                                  let mistura = ref [] in
                                  for x=0 to ((List.length (!retorno1))-1) do
                                      for w=0 to ((List.length (!retorno2))-1) do
                                          mistura := ([(List.nth (!retorno1) x);(List.nth (!retorno2) w)]@(!mistura))
                                      done;
                                  done;
                                  get_informacao := ((iterar_mistura (List.rev (!mistura)) (List.rev (!mistura)) regras [] 0)@(!get_informacao))
                          done;
                        lista_numeros_aux := (List.tl (!lista_numeros_aux));
                        maquina_aux := ([cabeca_da_cabeca,cabeca_numeros,(!get_informacao),(!tuplante)]@(!maquina_aux))
                done;
            maquina := (!maquina_aux);
            nova_lista_numeros := (!lista_numeros_aux)
        done;
        (!maquina) 

(* --------------------------------------------------------------------------------------------------------------- *)

let rec leitura_linhas n lista =
if n = 0 then
lista
else
let linha = read_line() in
leitura_linhas (n-1) (linha::lista)

let leitura() = 
  let palavra = sscanf (read_line()) "%s" (fun x->x) in
  let nr_regras = sscanf (read_line()) "%d" (fun x->x) in
  let regras = ref [] in
    regras := (leitura_linhas nr_regras []);
  (!regras,palavra)

(* --------------------------------------------------------------------------------------------------------------- *)

let get_maquina regras palavra =
let regras_fixed = ref [] in
    for i=0 to ((List.length regras)-1) do
        let sub_regras = explode (List.nth regras i) in
        if (List.length sub_regras) = 6 then
        regras_fixed := ([List.hd sub_regras,[String.make 1 (List.nth sub_regras 5)]]@(!regras_fixed))
        else
        regras_fixed := ([List.hd sub_regras,[(String.make 1 (List.nth sub_regras 5)) ^ (String.make 1 (List.nth sub_regras 7))]]@(!regras_fixed))
    done;
let letras = explode palavra in
let letras_rev = (List.rev letras) in
let first_half = ref [] in
    for i=1 to (List.length letras_rev) do
        let operador = ref [] in
        let letras_aux = ref letras_rev in
            for j=0 to (((List.length letras_rev)-(i-1))-1) do
                let operador_aux = ref [] in
                for p=0 to (i-1) do
                    let k = List.nth (!letras_aux) p in
                    operador_aux := (k::(!operador_aux));
                done;
                operador := ((!operador_aux)::(!operador));
                letras_aux := (List.tl (!letras_aux))
            done;
        first_half := ([(!operador)]@(!first_half))
    done;
let second_half = ref [] in
  let decrementador = ref (List.length letras) in
    for i=1 to (List.length letras) do
        let operador = ref [] in
        let incrementador = ref 1 in
        for j=1 to (!decrementador) do
            operador := ((i,(!incrementador))::(!operador));
            incrementador := ((!incrementador)+1)
        done;
        second_half := ((!operador)@(!second_half));
        decrementador := ((!decrementador)-1)
    done;
let maquina = make_maquina (List.rev (!first_half)) (List.rev (!second_half)) (!regras_fixed) in
(maquina,letras)

(* --------------------------------------------------------------------------------------------------------------- *)

let rec falso maquina elemento =
  match maquina with
  | [] -> 0
  | h::t -> let um,dois,tres,quatro = h in
            let one,two = dois in
            if elemento = one && tres <> [] then
            0
            else
            if elemento = one && tres = [] then
            falso t elemento 
            else
            1  

 let rec negativo maquina elemento =
  if List.hd maquina = elemento then -1 else
  match maquina with
  [] -> -1
  | h::t -> let one,two,three,four = h in
            let um,dois = two in
            if dois = 1 && three = [] then
            if (falso t um)=1 && (falso t (um+1))=1 then um-1
            else
            negativo t elemento
            else
            negativo t elemento

let corrigir_valor valor letras = 
    if valor = - 1 then 2 else
    ((List.length letras)-(valor-1))

let get_final maquina letras =
    let previo_valor = negativo (List.rev maquina) (List.hd maquina) in
    let valor = corrigir_valor previo_valor letras in
    let one,two,three,four = (List.hd maquina) in
    let normalizada = normalize three in
    let para_int = ref [] in
    for i=0 to ((List.length normalizada)-1) do
        let convert_to_int = int_of_char (List.nth normalizada i) in
        para_int := (convert_to_int::(!para_int))
    done;
    let inteiros = (!para_int) in
    let sort_para_int = (List.sort compare inteiros) in
    let para_char = ref [] in
    for i=0 to ((List.length sort_para_int)-1) do
        let convert_to_char = char_of_int (List.nth sort_para_int i) in
        para_char := (convert_to_char::(!para_char))
    done;
    let final = (!para_char) in
    let tamanho = (List.length letras) in
    let buliano = ref 0 in
    for i = 0 to ((List.length maquina)-1) do
        let one,two,three,four = (List.nth maquina i) in
        let um,dois = two in
        if um = tamanho && dois=1 && (List.mem 'S' three) then buliano := 1
        else nothing := ((!nothing)+1)
    done;
    let verdade = (!buliano) in
    (final,valor,verdade)

(* --------------------------------------------------------------------------------------------------------------- *)

let print_aux lst =
    if (List.length lst) = 0 then Printf.printf "" else
    for i=0 to ((List.length lst)-1) do
        Printf.printf "%c " (List.nth lst i)
    done;
    Printf.printf ""

let print_values maquina letras verdade finais valor =
    let () = if verdade = 1
            then 
            printf "YES\n"
            else
            printf "NO\n" in
    let () = if verdade = 1
            then (
                if (List.length finais) = 0 then Printf.printf "" else
                for i=0 to ((List.length finais)-1) do
                    if i=((List.length finais)-1) then
                    Printf.printf "%c\n" (List.nth finais i)
                    else 
                    Printf.printf "%c " (List.nth finais i)
                done; )
            else printf "%d\n" valor in
    0

(* --------------------------------------------------------------------------------------------------------------- *)

let main() =
    let regras,palavra = leitura() in
    let maquina,letras = get_maquina regras palavra in
    let finais,valor,verdade = get_final maquina letras in
    let finais_sorted = List.sort compare finais in
    let _a = print_values maquina letras verdade finais_sorted valor in
    0

(* --------------------------------------------------------------------------------------------------------------- *)

let _inicio = main()



