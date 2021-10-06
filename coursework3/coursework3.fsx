(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 3: Discriminated unions, higher-order functions, records

  ------------------------------------
  Name:
  Tallinn University of Technology Student ID:
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework3/coursework3.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Your solution should not contain the keywords 'rec' and 'mutable'.

  Deadline for submission is October 8.

  Please check the Moodle page of the course for updates:

  https://moodle.taltech.ee/course/view.php?id=31760

*)

// The task is to model an object that can move on a plane where
// coordinates are specified as integers.

// A position of an object is given as a pair of x and y coordinates as type XY:

type XY = int * int


// The object also has a direction. The possible directions are given as
// an enumeration (a simple version of discriminated unions).

type Dir =
    | N
    | E
    | S
    | W


// If the object moves, then it moves in its current direction.

// Note that moving in a given direction means:
// N - moving in parallel and in the direction of the y axis
// E - moving in parallel and in the direction of the x axis
// S - moving in parallel and in the opposite direction of the y axis
// W - moving in parallel and in the opposite direction of the x axis


// We also keep track of the history of the movement of the object.

type History = XY list



// We represent the object by the following record type. In other words,
// we track the current state (position and direction) and the history of
// the object.

type State =
    { position: XY
      direction: Dir
      history: History }



// The possible ways in which we can command the object to move on the
// plane is given by the following discriminated union:

type Command =
    | Step of int
    | Turn of int
    | Loop of int * int

// The meaning of the commands is the following:
// Step n      - the object must move n steps in the current direction;
// Turn n      - the object must turn right n quarter turns, e.g.,
//               when the current direction is N, then Turn 3 indicates
//               that the new direction must be W;
// Loop (m, n) - the object must make a rectangular "loop" with dimensions m and n, e.g.,
//               Loop (m, n) indicates that the object must:
//               - take m steps (in the current direction)
//               - turn right
//               - take n steps
//               - turn right
//               - take m steps
//               - turn right
//               - take n steps
//               - turn right


// You are given the function
//
// iterate : int -> ('a -> 'a) -> 'a -> 'a
//
// that will take a function f as an argument and compose it with
// itself n times.
//
// In other words,
//
// iterate 5 f x
//
// applies the function f to x five times, i.e., f (f (f (f (f x))))

let rec iterate (n: int) (f: 'a -> 'a) : 'a -> 'a =
    if n <= 0 then
        id
    else
        f << iterate (n - 1) f



// 1. We start by defining some basic functions.

// Define the function
//
// turn : Dir -> Dir
//
// that will compute the new direction after making one quarter turn to
// the right. A quarter turn is our unit of measurement for turning.

let turn (d: Dir) : Dir =
    match d with
    | N -> E
    | E -> S
    | S -> W
    | W -> N


// Define the function
//
// step : Dir -> XY -> XY
//
// that computes the new position after taking
// one step in the given direction.
//
// The distance between the new position and the old position must be 1.

let step (d: Dir) (xy: XY) : XY =
    match d with
    | N -> (fst xy, snd xy + 1)
    | E -> (fst xy + 1, snd xy)
    | S -> (fst xy, snd xy - 1)
    | W -> (fst xy - 1, snd xy)




// 2. We now define how to move the object. Define the function
//
// perform : (c : Command) -> (s : State) -> State
//
// that moves the object from state s according to c.
//
// Use the function iterate to perform the move, i.e.,
// Step n, Turn n and Loop (m, n) must be implemented using the functions
// iterate, step and turn.
//
// Note that it is perfectly valid for the object to step a negative
// amount in the current direction. Taking m steps in the current
// direction and then taking -m steps in the current direction should put
// the object back to where it was.
//
// Similarly, taking m turns followed by -m turns should result in the
// same direction that the object had before.
//
// You also need to track the history of the object.
// If performing a move makes the position of the object change,
// then you add the previous position to the path. For a Loop, you do this
// every time you reach a corner.
//
// The history is represented so that the most recent item is at the head.

let modBy x m = (x % m + m) % m

let modByFour x = modBy x 4

let getPosition (steps: int) (position: XY) (direction: Dir) : XY =
    match steps < 0 with
    | true -> (iterate (abs steps) (step (iterate 2 turn direction))) position
    | _ -> (iterate steps (step direction)) position

let matchLoop (m: int, n: int) (state: State) (index: int) : State =
    match index % 2 with
    | 0 ->
        match m with
        | 0 ->
            { direction = turn state.direction
              position = state.position
              history = state.history }
        | _ ->
            { direction = turn state.direction
              position = getPosition m state.position state.direction
              history = state.position :: state.history }
    | _ ->
        match n with
        | 0 ->
            { direction = turn state.direction
              position = state.position
              history = state.history }
        | _ ->
            { direction = turn state.direction
              position = getPosition n state.position state.direction
              history = state.position :: state.history }

let performCommand (c: Command) (s: State) : State =
    match c with
    | Step n ->
        match n with
        | 0 -> s
        | _ ->
            { direction = s.direction
              position = getPosition n s.position s.direction
              history = s.position :: s.history }
    | Turn n ->
        match n with
        | 0 -> s
        | _ ->
            { direction = iterate (modByFour n) turn s.direction
              position = s.position
              history = s.history }
    | Loop (m, n) -> [ 0 .. 3 ] |> List.fold (matchLoop (m, n)) s


// let initialState =
//     { position = 0, 0
//       direction = N
//       history = [] }

// let command = Loop(3, 0)

// let newState = performCommand command initialState

// printfn "%A" newState



// 3. Define the function
//
// perform : Command list -> State -> State
//
// that performes the moves according to the given list of commands
// starting from the given initial state.
//
// This must be implemented using a fold over the list of moves. (You
// can choose whether to use fold or foldBack.)

let performCommands (cs: Command list) (s: State) : State =
    cs
    |> List.fold (fun state command -> performCommand command state) s



// 4. Define the function
//
// flipSteps : Command list -> Command list
//
// that transforms the given list of commands so that each command Step n
// is replaced with Step -n. Leave other commands the same.
//
// Implement this using List.map

let flipSteps (commands: Command list) : Command list =
    commands
    |> List.map
        (fun command ->
            match command with
            | Step n -> Step(n * -1)
            | n -> n)



// 5. Define the function
//
// flipTurns : Command list -> Command list
//
// that transforms the given list of commands so that each Turn n
// is replaced with Turn m so that performing Turn m makes the object
// face the opposite direction compared to what it would be facing
// after performing Turn n. Leave other commands the same.
//
// Implement this using List.map


let flipTurns (commands: Command list) : Command list =
    commands
    |> List.map
        (fun command ->
            match command with
            | Turn n -> Turn(n + 2)
            | n -> n)



// 6. Define the function
//
// singleSteps : Command list -> Command list
//
//
// that removes from the given list of commands all those that are of the
// form Loop (m, n) or that are of the form Step k where the absolute
// value of k is not equal to 1.

let singleSteps (commands: Command list) : Command list =
    commands
    |> List.filter
        (fun command ->
            match command with
            | Loop (_) -> false
            | Step n when (abs n) <> 1 -> false
            | _ -> true)


// 7. Define the function
//
// unpackLoops : Command list -> Command list
//
// that replaces a command of the form Loop (m, n) with an equivalent
// list of commands consisting only of Step k and Turn l commands. Leave
// other commands the same.
//
// Implement this using List.collect

let unpack (index: int) (m: int) (n: int) : Command list =
    match index % 2 with
    | 0 -> [ Step m; Turn 1 ]
    | _ -> [ Step n; Turn 1 ]

let unpackLoops (commands: Command list) : Command list =
    commands
    |> List.collect
        (fun command ->
            match command with
            | Loop (m, n) -> List.concat [ for i in [ 0 .. 3 ] -> unpack i m n ]
            | _ -> [ command ])


// 8. Define the function
//
// simplify : Command list -> Command list
//
// that accumulates adjacent Step moves into a single Step
// and adjacent Turn moves into a single Turn. In other words,
// the resulting list of moves must not contain adjacent Step moves
// or adjacent Turn moves.
//
// The idea is to replace
// - adjacent Step m and Step n with Step (m + n)
// - adjacent Turn m and Turn n with Turn k so that
//   0 <= k <= 3 and
//   after performing Turn k the object is facing the same direction
//   as it would after performing Turn m followed by Turn n.
//
// Implement this using a fold (choose which one) over the input list.
// Do not traverse the input list several times.
//
// Here is a hint: the type of the accumulator (partial result) you use
// for the fold should allow you to keep track of at least two things:
// - The part of the list that is already simplified.
// - The part that you have seen but have not yet simplified. This is
//   the current sequence of (adjacent) moves of the same kind, i.e.,
//   a sequence of Step or a sequence of Turn moves. If the next move
//   you see is of a different kind, then you first simplify this part,
//   add it to the simplified part and then continue.

// let areCommandsSameType (first: Command) (second: Command) : bool =
//     match (first, second) with
//     | (Turn _, Turn _) -> true
//     | (Step _, Step _) -> true
//     | _,_ -> false

let simplify (commandList: Command list) : Command list =
    commandList
    |> List.fold
        (fun ((simplyfiedCommands, commands): Command list * Command list) (command: Command) ->
            match (command, commands.Head) with
            | (Turn m, Turn n) -> (simplyfiedCommands @ [ Turn(m + n) ], commands |> List.tail)
            | (Step m, Step n) -> (simplyfiedCommands @ [ Step(m + n) ], commands |> List.tail)
            | _, _ -> (simplyfiedCommands @ [ command ], commands |> List.tail))
        ([], commandList)
    |> fst
