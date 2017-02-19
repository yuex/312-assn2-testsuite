let val <
        foo= w : nat,
        baz= myVar : nat
        > =
            <
            foo=z,
            baz=z
            >
    val <
        bar = w : nat,
        zom = <nest= _ : <>, other = m : nat>,
        add = _ : nat @ myVar> =
            <
            bar = s(z),
            zom = <nest = <>, other= myVar>,
            add = s(s(z))
            >
in myVar
end
