(* Author: lazho
 * A program that builds a dictionary to represent a markov chain
 * based on given sample text. It can make random sentences that
 * half make sense.
 *)
module chatbot

(* A single node of a markov chain *)
type Node =
  | Root            // Start of sentence
  | Word of string  // Word in sentence
  | Leaf            // End of sentence

(* A node tupled with its frequency *)
type Branch = Node * int

(* A bunch of nodes tupled with their frequencies*)
type Branches = Branch list 

(* A node, tupled with all the nodes that can come after it *)
type Tree = Node * Branches

(* Full dictionary *)
type MarkovDict = Tree list

(* Retrieve a tree from the dictionary by its node *)
let rec getTree node dict =
  match dict with
    | [] -> failwith "How did you do this"
    | (n, bs)::ts ->
      if (node = n) then (n, bs)
      else (getTree node ts)

(* Calculate the sum of the frequencies of all the branches in a tree *)
let calcTreeSize (tree: Tree) =
  let (node, branches) = tree
  let rec helper branches =
    match branches with
      | [] -> 0
      | (n, f)::bs -> f+(helper bs)
  helper branches

(* Pick a random branch in a tree *)
let randomBranch (rand: System.Random) tree =
  let size = (calcTreeSize tree)
  let index = rand.Next(size)
  let rec helper cnt branches =
    match branches with
      | [] -> failwith "How did you do this"
      | [b] -> b
      | (n, f)::bs ->
        if (cnt < f) then (n, f)
        else (helper (cnt-f) bs)
  let (n, bs) = tree in
    (helper index bs)

(* Adds a new branch or increments the frequency of an existing branch *)
let rec updateBranches branches child =
  match branches with
    | [] -> [(child, 1)]
    | b::bs ->
      let (n, f) = b in
        if (n = child) then
          (n, f+1)::bs
        else b::(updateBranches bs child)

(* Adds a new tree or updates the frequency of an existing branch in a tree *)
let rec updateDict dict parent child =
  match dict with
    | [] -> [(parent, [(child, 1)])]
    | t::ts ->
      let (n, bs) = t in
        if (parent = n) then
          (n, (updateBranches bs child))::ts
        else t::(updateDict ts parent child)

(* Read a new line of text and add its words to the dictionary *)
let addLine (line: string) dict =
  let words = line.Split [|' '|] |> Array.toList
  let rec helper (p: Node) words dict =
    match words with
      | [] -> (updateDict dict p (Leaf))
      | w::ws ->
        (helper (Word(w)) ws (updateDict dict p (Word(w))))
  (helper (Root) words dict)

(* Make a dictionary from raw String *)
let makeDictFromString (raw: string) =
  let lines = raw.Split [|'\n'|] |> Array.toList
  let rec helper dict lines =
    match lines with
      | [] -> dict
      | l::ls -> (helper (addLine l dict) ls)
  (helper [] lines)

(* Make a dictionary from a text file *)
let makeDictFromFile (path: string) =
  let reader = new System.IO.StreamReader(path)
  let mutable dict = []
  let mutable line = reader.ReadLine()
  let mutable count = 0
  let mutable oldLength = 0
  while (not (line=null)) do
    dict <- (addLine line dict)
    line <- reader.ReadLine()
    count <- count+1
    if ((count%100)=0) then
      (printfn "%i lines read. Dictionary has %i trees (increased by %i)." count dict.Length (dict.Length-oldLength))
      oldLength <- dict.Length
  (printfn "All %i lines read. Dictionary has %i trees (increased by %i)." count dict.Length (dict.Length-oldLength))
  dict

(* Make a sentence given a dictionary and an RNG *)
let makeSentence rand dict =
  let rec helper acc p =
    match (randomBranch rand (getTree p dict)) with
      | (Leaf, _) -> acc
      | (Word(w), _) ->
        if (acc="") then (helper w (Word(w)))
        else (helper (acc+" "+w) (Word(w)))
      | _ -> failwith "How did you do this?"
  (helper "" (Root))

(* Make n sentences given dictionary and RNG *)
let rec makeSentences rand dict n =
  if (n <= 0) then ()
  else (printfn "%s" (makeSentence rand dict)); (makeSentences rand dict (n-1))

(* Main function
 * Usage: chatbot.exe [path_to_sample_file] [number_of_sentences]
 *)
[<EntryPoint>]
let main args =
  let path = args.[0]
  let n = args.[1] |> System.Int32.Parse
  let chatbot = (makeDictFromFile path)
  let rand = System.Random()
  makeSentences rand chatbot n
  0
