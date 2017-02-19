let
  val g : [two :: <one :: nat, two :: nat -> nat>, one :: nat] =
        in[one :: nat, two :: <one :: nat, two :: nat -> nat >]{two}(
                              <the = z, two = fn (x : nat) s(s(x)), one = z>
        )
in
  case g {
    one x : nat => s(x)
  | two <two = g : nat -> nat, one= _ : nat> => g z
  }
end
