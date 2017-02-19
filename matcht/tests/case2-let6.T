let
  val g : [two :: <one :: nat, two :: nat -> nat>, one :: nat] =
        in[one :: nat, two :: <one :: nat, two :: nat -> nat >]{two}(
                              <two = fn (x : nat) s(s(x)), one = z>
        )
in
  case g {
    one x : nat => s(x)
  | two <one= _ : nat, two = g : nat -> nat> => g z
  }
end
