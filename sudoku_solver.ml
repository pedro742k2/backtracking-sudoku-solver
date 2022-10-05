open Printf
open Scanf

(* Obter tamanho do tabuleiro *)
let grid_len = read_int()
(* Validar o tamanho do tabuleiro *)
let () = if grid_len < 4 || grid_len > 6 then raise (Invalid_argument "O tamanho do tabuleiro deve estar compreendido entre 4 e 6.")
(* Gerar a matriz do tabuleiro com todas as posições a zero, ou seja, por definir *)
let grid = Array.make_matrix grid_len grid_len 0

(* Obter número de condições *)
let ineqs_number = read_int()

(* Validar o nº de condições *)
let () = if ineqs_number < 0 then raise (Invalid_argument "O nº de condições deve nulo ou positivo.")

(* Gerar a matriz das restrições com o tamanho adequado ao número das mesmas, inicializando todos os valores com os tuplos (0, 0) *)
let ineqs = Array.make_matrix ineqs_number 2 (0, 0)

(* Gerar o vetor que armazenará temporariamente as restrições, par a par *)
let ineqs_input = Array.make ineqs_number (0, 0, 0, 0)

(* Obter as restrições a aplicar *)
let () = for i = 0 to ineqs_number-1 do
    let () = sscanf (read_line ()) " %d %d %d %d" (fun a b c d -> ineqs_input.(i) <- (a, b, c, d)) in
    let () = match ineqs_input.(i) with
      (* Verificar se as restrições se limitam ao tamanho do tabuleiro *)
      | (a, b, c, d) -> if a < 0 || b < 0 || c < 0 || d < 0 || a > grid_len-1 || b > grid_len-1 || c > grid_len-1 || c > grid_len-1 then raise (Invalid_argument "As condições devem-se restringir ao espaço do tabuleiro.") else
          (* Armazenar as restrições no respetivo vetor *)
          let () = ineqs.(i).(0) <- (a, b) in
          let () = ineqs.(i).(1) <- (c, d) in ()
    in ()
  done; ()

(* Procura pela próxima posição a preencher. Caso já tenha sido tudo preenchido, retorna o tuplo (-1, -1) *)
let rec find_empty row col =
  (* If at the end, all positions are filled *)
  if row > grid_len-1 then (-1, -1) else
    (* If at the end of the row, jump to new row *)
  if col > grid_len-1 then find_empty (row+1)(0) else
  if grid.(row).(col) = 0 then (row, col) else find_empty(row)(col+1)

(* Verifica se o valor pretendido já se encontra na respetiva linha *)
let rec check_valid_row row num = function
  | i when i > grid_len-1 -> true
  | i -> if grid.(row).(i) = num then false else check_valid_row row num (i+1)

(* Verifica se o valor pretendido já se encontra na respetiva coluna *)
let rec check_valid_col col num = function
  | i when i > grid_len-1 -> true
  | i -> if grid.(i).(col) = num then false else check_valid_col col num (i+1)

(* Verifica todas as inequações a cada validação, percorrendo o vetor das inequações para esse efeito *)
let rec check_ineqs i state =
  (* Caso o estado (state) seja false, retorna false *)
  if state = false then false else

    (* Caso chegue ao fim da iteração (por recursividade terminal) sem que state seja false, retorna true *)
  if i > ineqs_number-1 then true else

    (* p0 (posição à esquerda da desigualdade) deve ser maior do que p1 (posição à direita da desigualdade) *)
    let p0 = ineqs.(i).(0) in
    let p1 = ineqs.(i).(1) in

    match p0, p1 with
    | (a, b), (c, d) ->
      (* Obter o valor do tabuleiro na posição p0 (v0) e na posição p1 (v1). v0 > v1 para que a condição seja satisfeita *)
      let v0 = grid.(a).(b) in
      let v1 = grid.(c).(d) in
      (* Caso os valores já tenham sido preenchidos (ou seja, v0 e v1 diferentes de 0) e v0 < v1, então há pelo menos uma condição não satisfeita *)
      if (v0 <> 0 && v1 <> 0 && v0 <= v1) then check_ineqs(i+1) false else check_ineqs (i+1) true

(* Verifica se o valor dado pela função "solve" verifica todas as condições *)
let valid row col num = check_valid_row row num 0 && check_valid_col col num 0 && check_ineqs 0 true

(* Função que resolve o jogo *)
let rec solve(num) =
  (* Caso não consiga resolver com nenhum número no intervalo: {1, ..., grid_len} retorna false *)
  if num > grid_len then false else

    (* Procura a primeira posição por preencher *)
    let find = find_empty 0 0 in

    match find with
    (* Caso todas as posições estejam preenchidas, o tabuleiro foi preenchido com sucesso (caso "find_empty" retorne "(-1, -1)") *)
    | (-1, -1) -> true
    | (row, col) ->
      (* Verifica se o valor a inserir satisfaz todas as condições *)
      if valid(row)(col)(num) then
        (* Coloca o valor na respetiva posição *)
        let () = grid.(row).(col) <- num in

        (* Se conseguir resolver o tabuleiro, retorna true (Fazendo um reset na contagem). Caso contrário, executa o algoritmo de backtracking voltando uma posição atrás *)
        if solve(1) then true else let () = grid.(row).(col) <- 0 in (); solve(num+1)

      (* Caso o valor não seja válido, tenta o próximo valor disponível *)
      else solve(num+1)

(* Obtém o resultado da resolução (true se for resolvido com sucesso, false caso contrário) *)
let result = solve(1)

let () = if result then
    (* Imprime as posições preenchidas, caso resolvido com sucesso *)
    let () = for i = 0 to grid_len-1 do
        Array.iteri (fun i x -> if i < grid_len-1 then printf "%d " x else printf "%d" x) grid.(i); print_newline()
      done; in ()
    (* Imprime "IMPOSSIBLE", caso impossível de resolver *)
  else printf "IMPOSSIBLE\n"

