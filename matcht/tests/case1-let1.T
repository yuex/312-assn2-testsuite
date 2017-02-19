let
  val g : [TWO :: nat, ONE :: <>] = in[ONE :: <>, TWO :: nat]{TWO}(z)
in
  case g {
    ONE m : <> => z
  | TWO w : nat => s(z)
  }
end
