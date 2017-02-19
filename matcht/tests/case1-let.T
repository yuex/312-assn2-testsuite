let
  val g : [ONE :: <>, TWO :: nat] = in[ONE :: <>, TWO :: nat]{TWO}(z)
in
  case g {
    ONE m : <> => z
  | TWO w : nat => s(z)
  }
end
