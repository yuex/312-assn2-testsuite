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
        zom = <other = m : nat, nest= _ : <>>,
        add = _ : nat @ myVar> =
            <
            bar = s(z),
            add = s(s(z)),
            zom = <nest = <>, other = myVar>
            >
in myVar
end
