(*
  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------
  Coursework 0: Getting started
  ------------------------------------
  Name: Adam Harnúšek
  Student ID: 214374IV
  ------------------------------------
  Answer the questions below.  You answers to questions 2--8 should be
  correct F# code written after the question. The F# code for question
  1 is written for you and serves as an example. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.
  This coursework will NOT be graded but we encourage you to do it,
  you will not succeed in this course if you don't practice, and
  there's no time like the present! Also, you may find that parts of
  it appear in later courseworks. *)

// 0. Find your way to the fsharp interactive (fsi) command prompt.
// I.e. log in to a lab machine and start Visual Studio, install
// VSCode/Ionide and .net 5.0 on your laptop, etc.



// 1. Load the  following function into fsi
let greeting name = printfn "Hello: %s" name

// 2. Run the function greeting and  say hello to yourself.
greeting "Adam"

// 3. Create a value myName : string that contains your name.
let myName = "Adam"

// 4.Define
// splitAtChar : text:string -> sep:char -> list<string>
// is equivalent to
// splitAtChar : text:string -> sep:char -> string list
let splitAtChar (text: string) (sep: char) : list<string> = text.Split(sep) |> Array.toList

// 5. Write a function splitAtSpaces in such a way that it uses splitAtChar
// Hint: we defined splitAtSpaces in the lecture, now you need to modify it.
let splitAtSpaces (text: string) : list<string> = splitAtChar text ' '

// 6. Define sentenceCount : text:string -> int
let sentenceCount (text: string) : int = (splitAtChar text '.').Length

// 7. Define stats : text:string -> unit
// which prints the same stats as showWordCount and
// the number of sentences and average length of sentences
// hint: try float: int -> float
let showWordCount (text: string) = (splitAtSpaces text).Length

let stringLength (text: string) = text.Length

let stats (text: string) : unit =
    printfn "%d" (showWordCount text)
    let sentences = splitAtChar text '.'
    printfn "%d" sentences.Length

    let average =
        (sentences |> Seq.sumBy showWordCount |> float)
        / (sentences.Length |> float)

    printfn "%f" average

let picovina =
    "F# is a mature, open source, functional-first programming language which empowers users and organizations to tackle complex computing problems with simple, maintainable and robust code. It is used in a wide range of application areas and is available across multiple platforms. F# runs on Linux, Mac OS X, Android, iOS, Windows as well as HTML5 and GPUs. F# is free to use and has an OSI-approved open-source license. F# is supported by industry leading companies providing professional tools, and by an active open source community. The F# Software Foundation exists to promote, protect, and advance F#, and to support and foster the growth of a diverse international community of F# users."

stats picovina


// 8. Use the 'http' function from the lecture to download the file
// http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt as a string
// NOTE: you cannot use this function in tryfsharp. Instead you can
// paste the text into your file as a string and process it locally
open System.IO

let http (url: string) =
    let req = System.Net.WebRequest.Create url
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html



// 9. run stats on the downloaded file
stats (http "http://dijkstra.cs.ttu.ee/~juhan/itt8060/text.txt")
