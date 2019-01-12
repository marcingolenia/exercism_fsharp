module Allergies

open System

[<FlagsAttribute>]
type Allergen =
  | Eggs = 1
  | Peanuts = 2
  | Shellfish = 4
  | Strawberries = 8
  | Tomatoes = 16
  | Chocolate = 32
  | Pollen = 64
  | Cats = 128

let allergicTo codedAllergies (allergen: Allergen) =
  int (enum<Allergen>(codedAllergies) &&& allergen) <> 0

let list codedAllergies =
  let decodedAllergies = enum<Allergen>(codedAllergies)
  Enum.GetValues(typeof<Allergen>) :?> Allergen[]
    |> Array.filter (fun allergen -> decodedAllergies.HasFlag(allergen))
    |> Array.toList
