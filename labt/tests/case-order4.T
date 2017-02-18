(fn (g : [NONE :: <>, ALL :: nat, SOME :: nat])
  case g {
    NONE m => z
  | ALL  m => s(s(m))
  | SOME w => s(z)
  }
 )(in[SOME :: nat, NONE :: <>, ALL :: nat]{ALL}(z))
