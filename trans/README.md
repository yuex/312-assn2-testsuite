# Fix for trans/tests.sml

    cd trans
    patch -p1 < patch-trans-tests.diff

# Try Extra Test for Trans

    cd trans
    ln -sf ../testsuite/trans/tests.sml tests.sml
    sml ../testsuite/trans/run.sml
