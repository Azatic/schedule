//$ ./appendo.exe
//fun q -> fun r -> appendo q r (list (!!) [1; 2; 3; 4]), all answers {
//q=[]; r=[1; 2; 3; 4];
//q=[1]; r=[2; 3; 4];
//q=[1; 2]; r=[3; 4];
//q=[1; 2; 3]; r=[4];
//q=[1; 2; 3; 4]; r=[];
//}
//$ ./main.exe
//fun q -> substo (v varX) varX (v varY) q, 1 answer {
//q=V ("y");
//}
//fun q -> evalo (abs varX (v varX)) q, 1 answer {
//q=Abs ("x", V ("x"));
//}
//fun q -> evalo (abs varX (v varX)) q, 2 answers {
//q=Abs ("x", V ("x"));
//}
//fun q -> evalo (app (abs varX (v varX)) (v varY)) q, 1 answer {
//q=V ("y");
//}
//fun q -> evalo (app (abs varX (v varX)) q) (v varY), 1 answer {
//q=V ("y");
//}
//fun q -> evalo (app (abs varX q) (v varY)) (v varY), 1 answer {
//q=V ("x");
//}
//fun q -> evalo (app (v varX) (v varX)) q, 1 answer {
//q=App (V ("x"), V ("x"));
//}
//fun q -> evalo (v varX) q, 1 answer {
//q=V ("x");
//}
//fun q -> evalo (app q (v varX)) (v varX), 1 answer {
//q=Abs (_.44, V (_.44));
//}
//fun q -> fun r -> evalo (app r q) (v varX), 1 answer {
//q=V ("x"); r=Abs (_.54, V (_.54));
//}
//fun q -> fun r -> fun s -> a_la_quine q r s, 2 answers {
//q=Abs (_.668, V (_.668)); r=Abs (_.668, V (_.668)); s=Abs (_.668, V (_.668));
//q=Abs (_.783, V (_.783)); r=Abs (_.783, Abs (_.783, V (_.783))); s=Abs (_.783, Abs (_.783, V (_.783)));
//}



  $ ./for_ocaml_test.exe
  2021pi-1
   teorver1 diff1 Rpo
    Diff1 adf
    Matlog1 
     
  adf adf adf adf
  Solev
   teorver1 teorver2 
     
     
     
     
  2021pi-2
    teorver2 Rpo
    Diff1 
    Matlog1 
     
     
  Basov
    diff1 
    Diff1 
     
     
     
  zagl1
     Rpo
    Diff1 
    Matlog1 
     
     
  zagl2
     Rpo
    Diff1 
    Matlog1 
     
     
  Sartasov
     Rpo
     
     
     
     
  Starchak
     
     
    Matlog1 
     
     
