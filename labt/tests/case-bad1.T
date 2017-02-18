(fn (g : [NONE :: <>, SOME :: nat, ALL :: nat])
  case g {
    NONE m => z
  | SOME w => s(z)
  }
 )(in[NONE :: <>, SOME :: nat]{SOME}(z))
