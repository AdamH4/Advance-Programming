(*

  ITT8060 -- Advanced Programming 2021
  Department of Software Science
  Tallinn University of Technology
  ------------------------------------

  Coursework 2: Operations on lists and tuples, recursion, combination of functions

  ------------------------------------
  Name: Adam Harnúšek
  Tallinn University of Technology Student ID
  or Uni-ID: 214374IV
  ------------------------------------


  Answer the questions below.  You answers to all questions should be
  correct F# code written after the question. This file is an F#
  script file, it should be possible to load the whole file at
  once. If you can't then you have introduced a syntax error
  somewhere.

  This coursework will be graded. It has to be submitted to the https://gitlab.cs.ttu.ee
  repository itt8060-2021 under your name, into a file coursework2/coursework2.fsx.

  NB! Note that the solution has to be an F# script file!

  If the location, extension or name of the submission file or directory is incorrect it will not be graded.

  Deadline for submitting the solution is September 24 AoE, 2021.
*)

// You are given a type BibliographyItem that has the following structure:
// string list * string * (int * int) * int
// The meaning of the tuple elements is as follows:
// * The first field represents the list of author names where each name is in the format
//   "Lastname, Firstname1 Firstname2" (i.e. listing all first names after comma)
// * The second field represents the title of the publication
// * The third field represents a pair containing the starting page number and ending page number of the paper.
// * The fourth field represents the year of publication

type BibliographyItem = string list * string * (int * int) * int

// 1. Create a value bibliographyData : BibliographyItem list that contains
// at least 7 different publications on your favourite topic from https://dblp.uni-trier.de/
// Please note that you need not read the whole papers, just pick 7 papers that look interesting to you from the database.

let bibliographyData: BibliographyItem list =
    [ ([ "Liu, Shuguang"
         "Li, Zheng"
         "Ba, Lin" ],
       "Impact of Artificial Intelligence 2.0 on Teaching and Learning.",
       (128, 133),
       2020)
      ([ "Liu, Shuguang"
         "Li, Zheng"
         "Shi, Yungui"
         "Ba, Lin" ],
       "Reconstruction of Engineering Education Talent Training Model under the Background of Professional Certification.",
       (173, 177),
       2020)
      ([ "K, Raju"; "Chiplunkar, Niranjan N." ],
       "A survey on techniques for cooperative CPU-GPU computing.",
       (72, 85),
       2018)
      ([ "K, Akila"; "Chitrakala, S." ],
       "Highly refined human action recognition model to handle intraclass variability & interclass similarity.",
       (20877, 20894),
       2019)
      ([ "K., Gokila"; "Vennila, Ila" ],
       "A Secure Link Aware Fault Detection Algorithm for Enabling a Reliable Communication in MANET",
       (805, 820),
       2019)
      ([ "V, Prasanth"
         "Natarajan, Venki"
         "Basu, Kaushik" ],
       "Continuous Control Set Model Predictive Control of Buck Converter",
       (1297, 1302),
       2020)
      ([ "Kazi, Abdul Momin"
         "Ali, M."
         "K, Ayub"
         "Kalimuddin, H."
         "Zubair, K."
         "Kazi, Abdul Nafey"
         "A, Artani"
         "Ali, S. A." ],
       "Geo-spatial reporting for monitoring of household immunization coverage through mobile phones: Findings from a feasibility study.",
       (48, 55),
       2017) ]

// 2. Make a function compareLists : string list -> string list -> int that takes two string lists and
// returns
// * Less than zero in case the first list precedes the second in the sort order;
// * Zero in case the first list and second list occur at the same position in the sort order;
// * Greater than zero in case the first list follows the second list in the sort order;
// You need to use the support honouring the default culture for ordering strings, i.e. the built in
// compare does not work properly by default. Look at the
// documentation of the .Net comparison for strings: System.String.Compare
// If the first authors are the same
// then the precedence should be determined by the next author.
// Please note that your implementation should be recursive over the input lists.
//
// The sort order in .Net is defined using System.Globalization.CultureInfo:
// https://docs.microsoft.com/en-us/dotnet/api/system.globalization
// Please note that your solution should not force a particular sort order!

// string list * string * (int * int) * int
let authorsFromBibliographyItem (a: string list, _: string, _: (int * int), _: int) = a

let rec compareLists (first: string list) (second: string list) : int =
    match (first, second) with
    | ([], []) -> 0
    | ([], _) -> -1
    | (_, []) -> 1
    | (headFirst :: restFirst, headSecond :: restSecond) ->
        match System.String.Compare(headFirst, headSecond) with
        | 0 -> compareLists restFirst restSecond
        | x -> x

// compareLists
//     (authorsFromBibliographyItem (bibliographyData.Item(1)))
//     (authorsFromBibliographyItem (bibliographyData.Item(0)))


// 3. Make a function
// compareAuthors : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors.
// Use solution from task 3.
let compareAuthors (first: BibliographyItem) (second: BibliographyItem) : int =
    compareLists (authorsFromBibliographyItem first) (authorsFromBibliographyItem second)

// 4. Make a function
// compareAuthorsNumPages : BibliographyItem -> BibliographyItem -> int
// that takes two instances of bibliography items and compares them according to the authors and if the authors are
// the same then according to the number of pages in the publication.

let pagesFromBibliographyItem (_, _, a, _) = a

let sumPages (start: int, stop: int) : int = stop - start

let pagesCount = pagesFromBibliographyItem >> sumPages

let compareAuthorsNumPages (first: BibliographyItem) (second: BibliographyItem) : int =
    let isComparableByAuthors =
        compareLists (authorsFromBibliographyItem first) (authorsFromBibliographyItem second)

    match isComparableByAuthors with
    | 0 -> compare (pagesCount first) (pagesCount second)
    | _ -> isComparableByAuthors


// 5. Make a function
// sortBibliographyByNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the number of pages in the
// publication in ascending order.
// If two items are at the same level in the sort order, their order should be preserved.

let sortBibliographyByNumPages (data: BibliographyItem list) : BibliographyItem list = data |> List.sortBy pagesCount

// 6. Make a function
// sortBibliographyByAuthorNumPages : BibliographyItem list -> BibliographyItem list
// That returns a bibliography sorted according to the authors and number of pages in the publication in ascending order
// If two items are at the same level in the sort order, their order should be preserved.

let sortBibliographyByAuthorNumPages (data: BibliographyItem list) : BibliographyItem list =
    data
    |> List.sortBy (fun item -> authorsFromBibliographyItem item, pagesCount item)

// 7. Make a function
// groupByAuthor : BibliographyItem list -> (string * BibliographyItem list) list
// where the return list contains pairs where the first element is the name of a single
// author and the second element a list of bibliography items that the author has co-authored.
let filterBooksByAuthor (data: BibliographyItem list) (authorName: string) : (BibliographyItem list) =
    data
    |> List.filter (fun item -> List.contains authorName (authorsFromBibliographyItem item))

let groupByAuthor (data: BibliographyItem list) : (string * BibliographyItem list) list =
    data
    |> List.collect authorsFromBibliographyItem
    |> List.map (fun author -> (author, filterBooksByAuthor data author))
    |> List.distinct

// printfn "%A" (groupByAuthor bibliographyData)
