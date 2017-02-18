(fn (g : [NONE :: <>, SOME :: nat, ALL :: nat])
  case g {
    NONE m => z
  | ALL  m => s(s(m))
  | SOME w => s(z)
  }
 )(in[NONE :: <>, SOME :: nat, ALL :: nat]{ALL}(z))
