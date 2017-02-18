(fn (g : [NONE :: <>, ALL :: nat, SOME :: nat])
  case g {
    NONE m => z
  | SOME w => s(z)
  | ALL  m => s(s(m))
  }
 )(in[NONE :: <>, SOME :: nat, ALL :: nat]{ALL}(z))
