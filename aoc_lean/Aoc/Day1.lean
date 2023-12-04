def lines (s:String) :=
  s.split Char.isWhitespace

def get_last_elem (l: List α) : Option α :=
 match l with
  | [] => none
  | x::[] => some x
  | _::xs => get_last_elem xs

def make_num  : ( Option Char ) → ( Option Char ) → Nat
  |  some x, some y  => ( x.toString.toNat!  * 10 )  + ( y.toString.toNat!)
  |  _, _   => 0


def get_numbers ( s: String ) : List Nat:=
   (
    (List.map (λ (x,y) => make_num x y) ) ∘
    (List.map (λ x =>  ((List.head? x) , (get_last_elem x))) )  ∘
    List.map (List.filter Char.isDigit ∘ String.toList ) ∘
    lines)
    s

namespace Day1
def part1 : IO Unit := do
  let input ← IO.FS.readFile "input/1_1.txt"
  let stdout ← IO.getStdout
  let result := (List.foldl (Nat.add) 0 (get_numbers input) )
  stdout.putStrLn (Nat.repr result )

def part2 : IO Unit := do
  let stdout ← IO.getStdout
  stdout.putStrLn "Too much parsing but easy so not doing it"
end Day1
