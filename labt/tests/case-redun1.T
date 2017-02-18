(fn (g : [NONE :: <>, ALL :: nat, SOME :: nat])
  case g {
    NONE m => z
  | ALL  m => s(s(m))
  }
 )(in[NONE :: <>, ALL :: nat]{ALL}(z))
