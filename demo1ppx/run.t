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
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Sartasov Rpo
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Basov Diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Starchak Matlog1
  2021pi-1 2021pi-2 zagl1 zagl2 Burova Math
  2021pi-1 2021pi-2 zagl1 zagl2 Burova Math
  2021pi-1 2021pi-2 zagl1 zagl2 Burova Math
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 2021pi-2 zagl1 zagl2 Burova Math
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-1 2021pi-2 zagl1 zagl2 Burova Math
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Basov diff1
  2021pi-1 2021pi-2 zagl1 zagl2 Burova Math
  2021pi-1 Basov diff1
  2021pi-1 Solev teorver1
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Basov diff1
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-1 Solev teorver1
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-2 Solev teorver2
  2021pi-1 Basov diff1
  2021pi-2 Solev teorver2
  2021pi-1 Basov diff1
  2021pi-1 Basov diff1
  2021pi-2 Basov diff2
  0: [("2021pi-1", [[_.7006; "teorver1"; "Rpo"; "Diff1"]; [_.7822; "diff1"; "Matlog1"; "Math"]; [_.7104; _.7105; _.7106; _.7107]; [_.7169; _.7170; _.7171; _.7172]; [_.7225; _.7226; _.7227; _.7228]]); ("2021pi-2", [[_.10159; "diff2"; "Rpo"; "Diff1"]; [_.8649; "teorver2"; "Matlog1"; "Math"]; [_.8758; _.8759; _.8760; _.8761]; [_.8857; _.8858; _.8859; _.8860]; [_.8975; _.8976; _.8977; _.8978]]); ("zagl1", [[_.3876; _.3877; "Rpo"; "Diff1"]; [_.5687; _.5688; "Matlog1"; "Math"]; [_.4003; _.4004; _.4005; _.4006]; [_.4050; _.4051; _.4052; _.4053]; [_.4093; _.4094; _.4095; _.4096]]); ("zagl2", [[_.4219; _.4220; "Rpo"; "Diff1"]; [_.5690; _.5691; "Matlog1"; "Math"]; [_.4380; _.4381; _.4382; _.4383]; [_.4433; _.4434; _.4435; _.4436]; [_.4480; _.4481; _.4482; _.4483]]); ("Sartasov", [[_.736; _.737; "Rpo"; _.738]; [_.425; _.426; _.427; _.428]; [_.429; _.430; _.431; _.432]; [_.433; _.434; _.435; _.436]; [_.437; _.438; _.439; _.440]]); ("Basov", [[_.10162; "diff2"; _.10163; "Diff1"]; [_.8138; "diff1"; _.8140; _.8141]; [_.8205; _.8206; _.8207; _.8208]; [_.8296; _.8297; _.8298; _.8299]; [_.8394; _.8395; _.8396; _.8397]]); ("Starchak", [[_.1512; _.1513; _.1514; _.1515]; [_.2804; _.2805; "Matlog1"; _.2806]; [_.1544; _.1545; _.1546; _.1547]; [_.1557; _.1558; _.1559; _.1560]; [_.1608; _.1609; _.1610; _.1611]]); ("Burova", [[_.2947; _.2948; _.2949; _.2950]; [_.5693; _.5694; _.5695; "Math"]; [_.3046; _.3047; _.3048; _.3049]; [_.3103; _.3104; _.3105; _.3106]; [_.3177; _.3178; _.3179; _.3180]]); ("Solev", [[_.6060; "teorver1"; _.6062; _.6063]; [_.6648; "teorver2"; _.6649; _.6650]; [_.6109; _.6110; _.6111; _.6112]; [_.6123; _.6124; _.6125; _.6126]; [_.6135; _.6136; _.6137; _.6138]])]
  Execution time: 0.090316s
  fun q -> fun r -> appendo q r (list (!!) [1; 2; 3; 4]), all answers {
  q=[]; r=[1; 2; 3; 4];
  q=[1]; r=[2; 3; 4];
  q=[1; 2]; r=[3; 4];
  q=[1; 2; 3]; r=[4];
  q=[1; 2; 3; 4]; r=[];
  }
