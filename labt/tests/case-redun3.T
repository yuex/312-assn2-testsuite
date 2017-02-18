(fn (g : [NONE :: <>, ALL :: nat])
  case g {
    NONE m => z
  | ALL  m => s(s(m))
  | SOME m => s(s(z))
  }
 )(in[NONE :: <>, ALL :: nat]{ALL}(z))
