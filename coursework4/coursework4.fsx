(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 4: FSharpON

  ------------------------------------
  Name: Adam Harnúšek
  Tallinn University of Technology Student ID
  or Uni-ID: 214374IV
  ------------------------------------

  Answer the questions below. Your answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the
  https://gitlab.cs.ttu.ee repository itt8060-2021 under your name,
  into a file coursework4/coursework4.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or
  directory is incorrect it will not be graded.

  Your solution must not use mutable state and imperative features
  like for loops.

*)


(*

The ECMA-404 standard specifies a textual syntax for structured data
interchange.

The specification is available here:
https://www.ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf

The goal of this coursework is to develop a partial implementation of
this specification. In particular, our first goal is to define in F#
the datatype(s) suitable for representing the abstract syntax tree of
this data interchange format. The second goal is to define some
operations on this representation.

*)

// We have the following type alias.

type Name = string

type Value =
    | Object of (Name * Value) list
    | List of Value list
    | Number of float
    | String of string
    | Boolean of bool
    | Empty


//// Task 1 ////

// Define the type `Ecma` for representing the possible values of the
// ECMA-404 interchange format.
//
// The type must satisfy the constraint `equality`.

type Ecma = Value



// Define the following functions for creating ECMA-404
// representations of the given data.

(*
Define the function

  mkObject : unit -> Ecma

that creates a representation for an empty object structure.
*)

let mkObject () : Ecma = Object []

// match mkObject () with
// | Object _ -> printfn "%s" "Object"
// | List _ -> printfn "%s" "List"
// | _ -> printfn "%s" "else"
(*
Define the function

  mkNumber : float -> Ecma

that creates a representation for the given floating-point number.
*)

let mkNumber (num: float) : Ecma = Number num

(*
Define the function

  mkBool : bool -> Ecma

that creates a representation for the given Boolean value.
*)

let mkBool (value: bool) : Ecma = Boolean(value)

(*
Define the function

  mkString : string -> Ecma

that creates a representation for the given string value.
*)

let mkString (value: string) : Ecma = String(value)

(*
Define the function

 mkArray : Ecma list -> Ecma

that creates a representation for an array whose elements are
represented by the given list of `Ecma` values.
*)

let mkArray (list: Ecma list) : Ecma = List list

(*
Define the function

  mkNull : unit -> Ecma

that creates a representation of the ECMA-404 `null` value.
*)

let mkNull () : Ecma = Empty



//// Task 2 ////

// Define the function
//
//   addNameValue : Name * Ecma -> Ecma -> Ecma
//
// so that
//
//   addNameValue (n, v) e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an object representation
//
// - a representation for the object e extended with the name-value
//   pair (n, v), otherwise.

let addNameValue (n: Name, v: Ecma) (e: Ecma) : Ecma =
    match e with
    | Object object -> Object(object @ [ n, v ])
    | _ -> e


// Define the function
//
//   addValue : Ecma -> Ecma -> Ecma
//
// so that
//
//   addValue v e
//
// evaluates to an ECMA representation e' that is:
// - equal to e if e is not an array representation
//
// - a representation for the array e with the value v added as the last
//   element, otherwise.


let addValue (v: Ecma) (e: Ecma) =
    match e with
    | List list -> List(list @ [ v ])
    | _ -> e




//// Task 3 ////

// Define the function
//
//   countValues : Ecma -> int
//
// that counts the number of ECMA values in the given representation.
//
// Keep in mind that both objects and arrays are themselves values and
// may contain other values inside.
//
// Furthermore, the following should hold:
//
//   1 + countValues e <= countValues (addValue v e)             // if e is an array representation
//
//   1 + countValues e <= countValues (addNameValue (n, v) e)    // if e is an object representation

let rec countValues (e: Ecma) : int =
    match e with
    | List list ->
        (list
         |> List.fold (fun state item -> state + countValues item) 1)
    | Object object ->
        (object
         |> List.fold (fun state (_, value) -> state + countValues value) 1)
    | _ -> 1

//// Task 4 ////

type Path = Name list


// Define the function
//
//   listPaths : Ecma -> Path list
//
// that computes the full path for all the values in the given ECMA
// representation.
//
// A path is just a list of names that take us from the root of the
// representation to a particular value.
//
// For arrays, we consider the same path to take us to the array and to
// all of the elements in the array. Thus, for an array, we include the
// path to it and its elements only once in the result.
//
// If `e : Ecma` represents the following structure
//
//   {
//     "abc" : false,
//     "xs"  : [ { "a" : "a" }, 1.0, true, { "b" : "b" }, false ],
//     "xyz" : { "a" : 1.0,
//               "b" : { "b" : "b" } },
//     "ws"  : [ false ]
//   }
//
// then  `listPaths e` should result in
//
//   [
//     [];
//     ["abc"];
//     ["xs"];
//     ["xs"; "a"];
//     ["xs"; "b"];
//     ["xyz"];
//     ["xyz"; "a"];
//     ["xyz"; "b"];
//     ["xyz"; "b"; "b"];
//     ["ws"]
//   ]
//
// The ordering of paths in the result list matters:
// - paths to (sub)values in an array respect the order of elements in
//   the array
//
// - paths to values in an object respect the order in which the values
//   were added to the object (most recently added appears last).
//
// Note that the empty list denotes the path to the root object.

// let capitals =
//     [ "abc", Boolean false
//       "xs",
//       List(
//           [ Object([ "a", String "a" ])
//             Number 1.0
//             Boolean true
//             Object([ "b", String "b" ])
//             Boolean false ]
//       )
//       "xyz",
//       Object(
//           [ "a", Number 1.0
//             "b", Object([ "b", String "b" ]) ]
//       )
//       "ws", List([ Boolean false ]) ]

// let c =
//     [ "xyz",
//       Object(
//           [ "a", Number 1.0
//             "b", Object([ "b", String "b" ]) ]
//       ) ]

// let e: Ecma = Object(capitals)

// let rec traverseEcma (ecma: Ecma) : Path =
//     match ecma with
//     | Object object ->
//         match object.Length = 0 with
//         | true -> []
//         | _ ->
//             fst object.Head
//             :: traverseEcma (Object object.Tail)
//     | List list ->
//         match list.Length = 0 with
//         | true -> []
//         | _ ->
//             traverseEcma list.Head
//             @ traverseEcma (List list.Tail)
//     | _ -> []


// let rec listPaths (e: Ecma) : Path list =
//     match e with
//     | Object object ->
//         object
//         |> List.fold
//             (fun state (key, value) ->
//                 match value with
//                 | List list -> state @ [ key :: traverseEcma (List list) ]
//                 | Object ob -> state @ [ key :: traverseEcma (Object ob) ]
//                 | _ -> state @ [ [ key ] ])
//             []
//     | List list ->
//         list
//         |> List.fold
//             (fun state value ->
//                 match value with
//                 | List list -> state @ [ traverseEcma (List list) ]
//                 | Object ob -> state @ [ traverseEcma (Object ob) ]
//                 | _ -> state)
//             []
//     | _ -> [ [] ]

// let rec findRest (ecma: Ecma) : Path list =
//     match ecma with
//     | Object o ->
//         match o with
//         | start :: rest -> [ [ fst start ] ] @ findRest (Object rest)
//         | _ -> []
//     | List l ->
//         match l with
//         | start :: rest -> findRest start @ findRest (List rest)
//         | _ -> []
//     | _ -> []

// let rec loopEcma (e: Ecma) : Path list =
//     match e with
//     | Object object ->
//         match object with
//         | (name, value) :: tail ->
//             [ name ] :: findRest value
//             @ loopEcma (Object tail)
//         | _ -> []
//     | List list ->
//         match list with
//         | head :: tail -> loopEcma head @ loopEcma (List tail)
//         | _ -> []
//     | _ -> []

let listPaths (e: Ecma) : Path list =
    let rec innerlistPaths (e: Ecma) (prefix: Path) : Path list =
        match e with
        | Object ((name, value) :: rest) ->
            let newPrefix: Path = prefix @ [ name ]

            [ newPrefix ]
            @ innerlistPaths value newPrefix
              @ innerlistPaths (Object rest) prefix
        | List (head :: rest) ->
            innerlistPaths head prefix
            @ innerlistPaths (List rest) prefix
        | _ -> []

    [] :: innerlistPaths e []

// listPaths e

// let rec listPaths (e: Ecma) : Path list =
//     match e with
//     | Object object ->
//         object
//         |> List.fold (fun state item -> state @ [ [ fst item ] ] @ listPaths (snd item)) []
//     | List list ->
//         list
//         |> List.fold (fun state value -> state @ listPaths value) []
//     | _ -> [ [] ]

// listPaths e
// let rec lappend (state: Path list) (ecma: Ecma) : Path list =
//     match state, ecma with
//     | head :: tail, Object object ->
//         match object.Length = 0 with
//         | true -> []
//         | _ ->
//             let start = object.Head
//             let newItem: Path list = [ head @ [ fst start ] ]
//             printfn "%A" object.Head
//             printfn "%A" object.Tail
//             let newList = newItem @ tail
//             printfn "%A" newItem

//             newList
//             @ (lappend newList (snd start))
//               @ (lappend newList (Object object.Tail))
//     | x, List (start :: rest) -> (lappend x start) @ (lappend x (List rest))
//     | _, _ -> state

// let listPaths (e: Ecma) : Path list =
//     match e with
//     | Object object ->
//         object
//         |> List.fold (fun state item -> lappend state (Object [ item ])) [ [] ]
//         |> List.distinct
//     | _ -> [ [] ]


// let listPaths (e: Ecma) : Path list =
//     match e with
//     | Object object -> object |> List.fold (fun state item -> traverse) []
//     | List list -> []
//     | _ -> [ [] ]
//// Task 5 ////

// Define the function
//
//   show : Ecma -> string
//
// that computes a string representation of the given ECMA representation
// in such a way that the ordering requirements from the previous task are
// respected.
//
// The result should not contain any whitespace except when this
// whitespace was part of a name or a string value.


// TODO refactor and rename things
let rec show (ecma: Ecma) =
    match ecma with
    | Object o -> objectToJson o
    | List l -> listToJson l
    | Boolean b -> b.ToString().ToLower()
    | Number n -> n.ToString()
    | String t -> "\"" + t + "\""
    | Empty -> "null"

and objectToJson (object: list<Name * Ecma>) =
    match object with
    | [] -> "{}"
    | _ ->
        "{\""
        + fst object.Head
        + "\":"
        + show (snd (object.Head))
        + (object.Tail
           |> List.fold (fun res (name, e) -> res + "," + "\"" + name + "\":" + show e) "")
        + "}"

and listToJson (list: list<Ecma>) =
    match list with
    | [] -> "[]"
    | _ ->
        "["
        + show (list.Head)
        + (list.Tail
           |> List.fold (fun res e -> res + "," + show e) "")
        + "]"



//// Task 6 ////

// Define the function
//
//   delete : Path list -> Ecma -> Ecma
//
// so that
//
//   delete ps e
//
// evaluates to a representation `e'` that is otherwise the same as `e` but
// all name-value pairs with paths in the path list `ps` have been removed.
//
// When the user attempts to delete the root object, delete should throw
// an exception. Hint: use `failwith` in the appropriate case.

// let traverse (ecma: Value) (path: Path) : Value =


let rec folder (ecma: Ecma) (path: Path) : Ecma =
    match ecma with
    | Object object ->
        match path.Length = 1 with
        | true ->
            Object(
                object
                |> List.filter (fun (name, _) -> name <> path.Item(0))
            )
        | _ ->
            folder
                (object
                 |> List.find (fun (name, _) -> path.Head = name)
                 |> snd)
                path.Tail
    | _ -> ecma


let rec helper (e1: Ecma) (path: Path) (e2: Ecma) : Ecma =
    match e1, e2 with
    | Object e, Object result ->
        match path.Length = 1 with
        | true ->
            Object(
                e
                |> List.filter (fun (name, _) -> name <> path.Item(0))
                |> List.append result
            )
        | _ ->

            let nextLayer =
                (e
                 |> List.find (fun (name, _) -> path.Head = name)
                 |> snd)

            let keep =
                match nextLayer with
                | Object next ->
                    next
                    |> List.filter (fun (name, _) -> path.Head <> name)
                | List _ -> []
                | _ -> []

            let nieco =
                helper nextLayer path.Tail (Object result)

            match nieco with
            | Object goal -> Object(result @ keep @ goal)
            | _ -> e1
    | _ -> e1


let delete (paths: Path list) (e: Ecma) : Ecma =
    paths
    |> List.fold (fun state item -> helper e item state) (Object [])

// delete [ [ "xyz"; "a" ] ] (Object capitals)

//// Task 7 ////

// Define the function
//
//   withPath : Path list -> Ecma -> Ecma list
//
// so that
//
//   withPath ps e
//
// evaluates to a list of object representations consisting of those
// objects in `e` that are represented by a path from the list `ps`.
//
// The result list must respect the ordering requirements from Task 4.

// let appendToObjectEnd (e: Ecma) (path: Path) (key: Name, value: Value) : Ecma =
//     match e with
//     | Object (head :: tail) ->
//         match path with
//         | start :: rest ->
//             match (fst head) = start with
//             | true -> addNameValue
//             | _ -> e
//     | _ -> e

let appendToFinalEcma (state: Ecma, e: Ecma) (key: Name) : (Ecma * Ecma) =
    match e with
    | Object object ->
        let keep =
            object |> List.find (fun (name, _) -> name = key)

        addNameValue keep state, (Object [ keep ])
    | _ -> state, e

let traverse (path: Path) (e: Ecma) : Ecma =
    path
    |> List.fold appendToFinalEcma (mkObject (), e)
    |> fst

let withPath (paths: Path list) (e: Ecma) : Ecma list =
    paths
    |> List.fold (fun state path -> state @ [ traverse path e ]) []
