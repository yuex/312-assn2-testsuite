let val <
        baz= myVar : nat,
        foo= w : nat
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
            zom = <nest = <>, other = myVar>,
            bar = s(z),
            add = s(s(z))
            >
in myVar
end
