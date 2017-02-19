structure TestHarness :> TESTHARNESS =
struct
  structure T = MatchTChecker
  structure UD = MatchTDynamics
  structure TLI = TopLevelImpl
  structure TL = TopLevel

  open MatchT

  datatype 'a expected_result
    = Pass of 'a
    | Fail
    | TypeFail
    | ParseFail
    | EvalFail

  (* Tests cases are a list of commands an expected results *)

  val natt = Typ.Nat

  fun Num n =
      if (n = 0)
      then Term.Z'
      else Term.S' (Num (n - 1))

  type test = string * (Term.t expected_result)
  val tests : test list = []

  val testFiles = [
      ("../testsuite/matcht/tests/case1-let.T", Pass (Num 1)),
      ("../testsuite/matcht/tests/case1-let1.T", Pass (Num 1)),
      ("../testsuite/matcht/tests/case1-let2.T", Pass (Num 0)),
      ("../testsuite/matcht/tests/case1-let3.T", Pass (Num 1)),
      ("../testsuite/matcht/tests/case2-let.T", Pass (Num 0)),
      ("../testsuite/matcht/tests/case2-let1.T", Pass (Num 0)),
      ("../testsuite/matcht/tests/case2-let2.T", TypeFail),
      ("../testsuite/matcht/tests/case2-let3.T", TypeFail),
      ("../testsuite/matcht/tests/case2-let4.T", Pass (Num 1)),
      ("../testsuite/matcht/tests/case2-let5.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/case2-let6.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/case2-let7.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/case2-let8.T", TypeFail),
      ("../testsuite/matcht/tests/let1.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/let2.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/let3.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/let4.T", Pass (Num 2)),
      ("../testsuite/matcht/tests/let.T", Pass (Num 2))
  ]


  fun vprint verb s = if verb then print s else ()

  fun success verb n =
      (if verb then print ((Int.toString n) ^ ": Success!\n") else ();
       true)

  fun hdl verb n f comp exp =
      let val res = f () in
        (case exp of
             Pass(x) =>
             let
               val passed = comp(res,x)
             in
               if passed then success verb n
               else (vprint verb ((Int.toString n)
                                  ^ ": Failed: Result does not match\n") ; false)
             end
           | _ => ((vprint verb "Failed...\n") ; false))
      end
      handle
        Parser.Error =>
        (vprint verb ((Int.toString n) ^ ": Failed: Parse Error\n");
         false)
      | Parser.Unsupported str =>
        (vprint verb ((Int.toString n) ^ ": Failed: Unsupported operation: "
                      ^ str ^ "\n");
         false)
      | Bind.Error str =>
        (vprint verb ((Int.toString n) ^ ": Failed: " ^ str);
         false)
      | T.TypeError NONE =>
        (case exp of
             (Fail | TypeFail) => success verb n
           | _ => (vprint verb ((Int.toString n) ^ ": Failed: Type Error\n");
                   false))
      | T.TypeError (SOME term) =>
        (case exp of
             (Fail | TypeFail) => success verb n
           | _ => (vprint verb ((Int.toString n) ^ ": Failed: Type Error: Term: "
                                                 ^ Term.toString term ^ "\n");
                   false))
      | UD.RuntimeError =>
        (case exp of
             Fail => success verb n
           | EvalFail => success verb n
           | _ => (vprint verb ((Int.toString n) ^
                                ": Failed: Runtime Error\n") ; false))
      | UD.Malformed =>
        (vprint verb ((Int.toString n) ^
                      ": Failed: Malformed Error in InexhaustiveDynamics\n") ; false)
      (*| _ =>
        (vprint verb ((Int.toString n) ^
                      ": Failed: Other Exception\n") ; false) *)

  exception TestError

  (* Each eval evaluates to either a number or a string. A test takes an
   * eval command and an expected result of the corresponding type. *)

  fun runtest v ((text,exp_res),(L,n)) =
      ((hdl v n (fn () => TL.eval text)
            (fn (TLI.Val(t),x) => (Term.aequiv(t,x)) | _ => raise TestError) exp_res)::L,n+1)

  fun runtestFile v ((file, exp_res),(L,n)) =
      ((hdl v n (fn () => TL.evalFile file)
            (fn (TLI.Val(t),x) => (Term.aequiv(t,x)) | _ => raise TestError) exp_res)::L,n+1)

  (* Each step command either steps to another term, is a val
   * or results in an error. A steptest tests whether a command steps or not.
   * In the case of unchecked dynamics, an ill typed expression is expected
   * to result in a TypeFail.*)
  type steptest = string * (bool expected_result)
  val steptests = []

  fun runsteptest v ((text,exp_res),(L,n)) =
      ((hdl v n (fn () => TL.eval text)
            (fn (TLI.Next(t),x) => x | (_,x) => not(x)) exp_res)::L,n+1)

  fun summarize [] (pass,fail) =
    ((if fail > 0 then
        TextIO.print "-------------------------------------------------------\n"
      else ());
     TextIO.print ("\nTests completed: "
                   ^ (Int.toString (pass + fail)) ^ "\n");
     TextIO.print ("Tests passed   : " ^ (Int.toString pass)        ^ "\n");
     TextIO.print ("Tests failed   : " ^ (Int.toString fail)        ^ "\n");
     if fail = 0 then
       TextIO.print "Congratulations!\n"
     else
       () )
  | summarize (result::results) (pass,fail) =
    let
      val stats' = if result then (pass+1,fail) else (pass,fail+1)
    in
      summarize results stats'
    end

  fun runfiletests verbose =
      (print "\n\nRunning normal tests...\n";
       summarize (#1 (foldl (runtestFile verbose) ([],1) testFiles)) (0,0))

  fun runtests verbose =
      (print "\n\nRunning normal tests...\n";
       summarize (#1 (foldl (runtest verbose) ([],1) tests)) (0,0))
  fun runsteptests verbose =
      (print "\n\nRunning step tests...\n";
       summarize (#1 (foldl (runsteptest verbose) ([],1) steptests)) (0,0))

  fun runalltests v = (runtests v ; runsteptests v)
end
