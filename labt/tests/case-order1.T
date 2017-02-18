(fn (g : [NONE :: <>, SOME :: nat, ALL :: nat])
  case g {
    NONE m => z
  | SOME w => s(z)
  | ALL  m => s(s(m))
  }
 )(in[NONE :: <>, SOME :: nat, ALL :: nat]{ALL}(z))
