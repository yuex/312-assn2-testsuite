(fn (g : [NONE :: <NONE :: <>, SOME :: <>>, SOME :: nat])
  case g {
    NONE m => z
  | SOME w => s(z)
  }
 )(in[NONE :: <SOME :: <>, NONE :: <>>, SOME :: nat]{SOME}(z))
