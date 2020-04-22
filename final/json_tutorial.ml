(* Yojson Tutorial *)

(* This file depends on the Yojson package. So to [#use] this file in utop,
   you must [#require "yojson";;].  And to compile the file with ocamlbuild,
   you must either (i) have a _tags file that includes [PKG yojson] and pass
   the [-use-ocamlfind] flag to ocamlbuild, or (ii) not have a _tags file
   and instead pass the [-pkg yojson] flag to ocamlbuild. *)

(* The function [Yojson.Basic.from_file] takes a couple optional arguments
   we can ignore.  Let's think of its type as simply
   [string -> Yojson.Basic.t].  [from_file fn] reads the contents of the
   file whose name is [fn], and if they are well-formed JSON, it creates a
   tree representing those contents. *)
let j = Yojson.Basic.from_file "cornell.json"

(* Another helpful function is
   [Yojson.Basic.from_string : string -> Yojson.Basic.t], which
   reads the JSON directly from the string instead of a file. 
   That is very useful for constructing unit tests. *)

(* In utop you could now see the tree that was constructed from what
   was read in:

   # j;;
   - : Yojson.Basic.t =
   `Assoc
   [("university", `String "Cornell");
   ("people",
    `List
      [`Assoc
         [("first name", `String "Ezra"); 
          ("last name", `String "Cornell");
          ("arrived", `Int 1865)];
       `Assoc
         [("first name", `String "Martha"); 
          ("last name", `String "Pollack");
          ("arrived", `Int 2017)]])]

   The [Yojson.Basic.t] type (henceforth just [t] for short) is a tree that
   is defined as follows:

   type t = [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * t) list
    | `List of t list
   ]

   This type is a _polymorphic variant_, which you can read more about
   in the textbook.

   The leaves of a json tree can be booleans, floats, ints, strings,
   or null (i.e., nothing).  And there are two types of nodes, `List
   and `Assoc.  The `List node represents a JSON array (which is really
   just an ordered list of values, hence corresponds to the OCaml concept
   of a list), and the `Assoc node represents a JSON object (which is really
   an unordered collection of key/value pairs, hence corresponds to the
   OCaml concept of an association list, which you can read more about
   in the textbook).

   Take a minute to compare the value utop printed out above for [j] to
   the JSON file it came from, so that you can see how the information from
   the file is represented as a [Yojson.Basic.t] value.
*)

open Yojson.Basic.Util
(*
To extract information from a JSON tree, there are several useful
functions in [Yojson.Basic.util], whose documentation is here:
[https://ocaml-community.github.io/yojson/yojson/Yojson/Basic/Util/index.html].
The important ones for our purposes are:

   - [to_assoc : t -> (string * t) list].  If [j] is [`Assoc lst], then
     [to_assoc j] is just [lst].
   - [to_list : t -> t list].  If [j] is [`List lst], then
     [to_list j] is just [lst].
   - [to_string : t -> string].  If [j] is [`String s], then
     [to_string j] is just [s].
   - [to_int : t -> int].  If [j] is [`Int i], then
     [to_int j] is just [i].

All of those functions raise [Type_error] if their precondition fails.
Here are some examples:
*)

(* Extract the name of the university from the JSON *)
let the_university = to_string (List.assoc "university" (to_assoc j))
let () = assert (the_university = "Cornell")

(* The same, but written more idiomatically with pipeline *)
let the_university' = j |> to_assoc |> List.assoc "university" |> to_string
let () = assert (the_university' = "Cornell")

(* Demonstrate that a Type_error is raised when applying a function
   to the wrong type of json element *)
let _ = try
    let _ = to_list j in assert false
  with Type_error _ -> assert true (* j is an `Assoc not a `List *)

(*
  One other important function that combines [to_assoc] and
  [List.assoc] can simplify the code we just wrote:

     - [member : string -> t -> t].  If [j] is [`Assoc lst], and
       if [lst = [ ...; (k, v); ...]], and if that is the earliest occurrence
       of [k] in [lst], then [member k j] is [v].  If [k] is not a key
       in [lst], then [member k j] is [`Null].
*)

(* Demonstrate member *)
let the_university'' = j |> member "university" |> to_string
let () = assert (the_university'' = "Cornell")

(* Suppose we wanted to convert the entire JSON tree into a more meaningful,
   custom-designed OCaml type.  Here's how we could do it. *)

(* First, we define OCaml types to represent a university. *)

type person = {
  first_name : string;
  last_name : string;
  arrived : int;
}

type university = {
  name : string;
  people : person list;
}

(* Then we write some functions to _parse_ the JSON into that type. *)

let person_of_json j = {
  first_name = j |> member "first name" |> to_string;
  last_name = j |> member "last name" |> to_string;
  arrived = j |> member "arrived" |> to_int;
}

let university_of_json j = {
  name = j |> member "university" |> to_string;
  people = j |> member "people" |> to_list |> List.map person_of_json;
}

let parse j =
  try university_of_json j
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let u = parse j

(*
In utop you will see we now have a nice representation of the JSON:

# u;;
- : university =
  {name = "Cornell";
   people =
    [{first_name = "Ezra"; last_name = "Cornell"; arrived = 1865};
     {first_name = "Martha"; last_name = "Pollack"; arrived = 2017}]}
*)

(* Here's an example of how things could go wrong.  Suppose the
   JSON were missing a field named "university", perhaps because
   it had been misspelled: *)

let bad_json = {|{"univerzity" : "cornell", "people": []}|}

(* The construct {|...|} is another way to write an OCaml string, but it
   allows double quotes inside. *)

(* Now when we try to parse it, we get an error.  Uncomment the line
   below to see it. *)

(* let v = parse (Yojson.Basic.from_string bad_json) *)

(* The error is...
     Exception: Failure "Parsing error: Expected string, got null".
   That's because university_of_json tries to get the "university" field,
   which doesn't exist, so member returns `Null, to which to_string is applied,
   producing a Type_error that contains that error message.
*)
