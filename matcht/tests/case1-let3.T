let
  val g : [TWO :: nat, ONE :: <>] = in[ONE :: <>, TWO :: nat]{TWO}(z)
in
  case g {
    TWO w : nat => s(z)
  | ONE m : <> => z
  }
end
