// Generated by js_of_ocaml
//# buildInfo:effects=false, use-js-string=false, version=5.0.1
(function
   (globalThis)
   {"use strict";
    var
     runtime=globalThis.jsoo_runtime,
     caml_string_of_jsbytes=runtime.caml_string_of_jsbytes;
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    function caml_call2(f,a0,a1)
     {return f.length == 2?f(a0,a1):runtime.caml_call_gen(f,[a0,a1])}
    function caml_call3(f,a0,a1,a2)
     {return f.length == 3?f(a0,a1,a2):runtime.caml_call_gen(f,[a0,a1,a2])}
    var
     global_data=runtime.caml_get_global_data(),
     cst=caml_string_of_jsbytes(""),
     cst_type_lib=caml_string_of_jsbytes("type_lib"),
     cst_type_lib$0=caml_string_of_jsbytes("type_lib"),
     OCanren=global_data.OCanren,
     OCanren_List=global_data.OCanren__List,
     Ppx_inline_test_lib_Runtime=global_data.Ppx_inline_test_lib__Runtime,
     OCanren_Pair=global_data.OCanren__Pair,
     GT=global_data.GT;
    caml_call2(Ppx_inline_test_lib_Runtime[2],cst_type_lib,cst);
    function addo(k,v,map,map_new)
     {var
       _q_=caml_call2(OCanren[64][12],k,v),
       _r_=caml_call2(OCanren_List[101],_q_,map);
      return caml_call2(OCanren[33],_r_,map_new)}
    var IAssoc=[0,addo];
    function reifier(param)
     {var
       _o_=caml_call1(OCanren_List[105],OCanren[27]),
       _p_=caml_call1(OCanren_List[105],_o_);
      return caml_call1(OCanren_List[105],_p_)}
    var
     _a_=caml_call1(OCanren_List[105],OCanren[27]),
     _b_=caml_call1(OCanren_List[105],_a_),
     _c_=caml_call2(OCanren_Pair[97],OCanren[27],_b_),
     storage_reifier=caml_call1(OCanren_List[105],_c_),
     _d_=caml_call1(GT[221],GT[60]),
     _e_=caml_call1(caml_call1(GT[221],OCanren[19]),_d_),
     _f_=caml_call2(GT[221],OCanren_List[57],_e_),
     _g_=caml_call2(GT[221],OCanren_List[57],_f_),
     shower=caml_call2(GT[221],OCanren_List[57],_g_),
     _h_=caml_call1(GT[221],GT[60]),
     _i_=caml_call1(caml_call1(GT[221],OCanren[19]),_h_),
     _j_=caml_call2(GT[221],OCanren_List[57],_i_),
     _k_=caml_call2(GT[221],OCanren_List[57],_j_),
     _l_=caml_call1(GT[221],GT[60]),
     _m_=caml_call1(caml_call1(GT[221],OCanren[19]),_l_),
     _n_=caml_call3(GT[221],OCanren_Pair[57],_m_,_k_),
     show_storage=caml_call2(GT[221],OCanren_List[57],_n_);
    caml_call1(Ppx_inline_test_lib_Runtime[3],cst_type_lib$0);
    var Type_core=[0,IAssoc,reifier,storage_reifier,shower,show_storage];
    runtime.caml_register_global(8,Type_core,"Type_core");
    return}
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLjAsImZpbGUiOiIudHlwZV9saWIub2Jqcy90eXBlX2xpYi5jbWEuanMiLCJzb3VyY2VSb290IjoiIiwibmFtZXMiOlsiYWRkbyIsImsiLCJ2IiwibWFwIiwibWFwX25ldyIsInJlaWZpZXIiLCJzdG9yYWdlX3JlaWZpZXIiLCJzaG93ZXIiLCJzaG93X3N0b3JhZ2UiXSwic291cmNlcyI6WyIvVXNlcnMvYXphdC9EZXNrdG9wL215X3NjaGVkX3Byb2ovX2J1aWxkL2RlZmF1bHQvZGVtbzFwcHgvdHlwZV9jb3JlLm1sIl0sIm1hcHBpbmdzIjoiOzs7OztJOzs7Ozs7Ozs7SUEwQ0U7Ozs7Ozs7Ozs7O2FBUElBLEtBQUtDLEVBQUVDLEVBQUVDLElBQUlDO01BQXdCO3NDQUFoQ0gsRUFBRUM7T0FBZ0IscUNBQWRDO01BQWMsa0NBQVZDLFFBQXNEO2tCQUFuRUo7YUFHRks7TUFJOEI7O09BQWhCOzhDQUFGO0lBUW1DOztLQUFoQjtLQUE5QjtLQURIO0tBU3VEO0tBQWxCLGVBQVA7S0FBeEI7S0FGSDtLQUZIO0tBZTBEO0tBQWxCLGVBQVA7S0FBeEI7S0FGSDtLQUR5QjtLQUFsQixlQUFQO0tBRkg7S0FGSDs7SUFaaUQsd0JBWi9DQSxRQU9BQyxnQkFRQUMsT0FRQUM7O1UiLCJzb3VyY2VzQ29udGVudCI6WyJvcGVuIE9DYW5yZW5cbm9wZW4gT0NhbnJlbi5TdGRcblxudHlwZSBpc2NoZWR1bGUgPSBzdHJpbmcgaWxvZ2ljIFN0ZC5MaXN0LmluamVjdGVkIFN0ZC5MaXN0LmluamVjdGVkXG50eXBlIHNjaGVkdWxlX2xvZ2ljID0gc3RyaW5nIGxvZ2ljIFN0ZC5MaXN0LmxvZ2ljIFN0ZC5MaXN0LmxvZ2ljXG5cbnR5cGUgd2Vla19kYXkgPVxuICB8IE1vbmRheVxuICB8IFR1ZXNkYXlcbiAgfCBXZWRuZXNkYXlcbiAgfCBUaHVyc2RheVxuICB8IEZyaWRheVxuXG50eXBlIGdyb3VwX29yX3RlYWNoZXIgPVxuICB8IFRlYWNoZXIgb2YgeyBuYW1lIDogc3RyaW5nIH1cbiAgfCBHcm91cCBvZiB7IG51bWJlciA6IGludCB9XG5cbnR5cGUgdXNlcl9jb25zdHJhaW50cyA9XG4gIHwgR3JvdXBfbm90X2xlYXJuaW5nIG9mXG4gICAgICB7IG51bWJlcl9sZXNzb24gOiBpbnRcbiAgICAgIDsgZGF5IDogd2Vla19kYXlcbiAgICAgIH1cblxudHlwZSB1c2VyX2NvbnN0cmFpbnRzXzEgPVxuICB8IE5vdF9sZWFybmluZyBvZlxuICAgICAgeyBudW1iZXJfbGVzc29uIDogaW50XG4gICAgICA7IGRheSA6IHdlZWtfZGF5XG4gICAgICA7IGdyb3VwX29yX3RlYWNoZXIgOiBncm91cF9vcl90ZWFjaGVyXG4gICAgICB9XG5cbm1vZHVsZSBJQXNzb2MgPSBzdHJ1Y3RcbiAgdHlwZSBrZXkgPSBzdHJpbmcgaWxvZ2ljXG4gIHR5cGUgJ2EgdCA9IChrZXksICdhKSBTdGQuUGFpci5pbmplY3RlZCBTdGQuTGlzdC5pbmplY3RlZFxuXG4gICgqIGxldCBmaW5kbyA6IGtleSAtPiAnYSBpbG9naWMgdCAtPiAnYSBpbG9naWMgLT4gZ29hbCA9IG15YXNzb2NvICopXG4gIGxldCBhZGRvIGsgdiBtYXAgbWFwX25ldyA9IFN0ZC5MaXN0LmNvbnMgKFN0ZC5wYWlyIGsgdikgbWFwID09PSBtYXBfbmV3XG5lbmRcblxubGV0IHJlaWZpZXIgKClcbiAgOiAoICdhIGlsb2dpYyBTdGQuTGlzdC5pbmplY3RlZCBTdGQuTGlzdC5pbmplY3RlZCBTdGQuTGlzdC5pbmplY3RlZFxuICAsICdhIGxvZ2ljIFN0ZC5MaXN0LmxvZ2ljIFN0ZC5MaXN0LmxvZ2ljIFN0ZC5MaXN0LmxvZ2ljICkgUmVpZmllci50XG4gID1cbiAgU3RkLkxpc3QucmVpZnkgKFN0ZC5MaXN0LnJlaWZ5IChTdGQuTGlzdC5yZWlmeSBPQ2FucmVuLnJlaWZ5KSlcbjs7XG5cbmxldCBzdG9yYWdlX3JlaWZpZXJcbiAgOiAoIChJQXNzb2Mua2V5LCBpc2NoZWR1bGUpIFBhaXIuZ3JvdW5kaSBTdGQuTGlzdC5pbmplY3RlZFxuICAsIChzdHJpbmcgbG9naWMsIHNjaGVkdWxlX2xvZ2ljKSBQYWlyLmxvZ2ljIFN0ZC5MaXN0LmxvZ2ljICkgUmVpZmllci50XG4gID1cbiAgU3RkLkxpc3QucmVpZnlcbiAgICAoU3RkLlBhaXIucmVpZnkgT0NhbnJlbi5yZWlmeSAoU3RkLkxpc3QucmVpZnkgKFN0ZC5MaXN0LnJlaWZ5IE9DYW5yZW4ucmVpZnkpKSlcbjs7XG5cbmxldCBzaG93ZXIgPVxuICBHVC5zaG93XG4gICAgU3RkLkxpc3QubG9naWNcbiAgICAoR1Quc2hvd1xuICAgICAgIFN0ZC5MaXN0LmxvZ2ljXG4gICAgICAgKEdULnNob3cgU3RkLkxpc3QubG9naWMgKEdULnNob3cgT0NhbnJlbi5sb2dpYyBAQCBHVC5zaG93IEdULnN0cmluZykpKVxuOztcblxubGV0IHNob3dfc3RvcmFnZSA9XG4gIEdULnNob3dcbiAgICBTdGQuTGlzdC5sb2dpY1xuICAgIChHVC5zaG93XG4gICAgICAgU3RkLlBhaXIubG9naWNcbiAgICAgICAoR1Quc2hvdyBPQ2FucmVuLmxvZ2ljIEBAIEdULnNob3cgR1Quc3RyaW5nKVxuICAgICAgIChHVC5zaG93XG4gICAgICAgICAgU3RkLkxpc3QubG9naWNcbiAgICAgICAgICAoR1Quc2hvdyBTdGQuTGlzdC5sb2dpYyAoR1Quc2hvdyBPQ2FucmVuLmxvZ2ljIEBAIEdULnNob3cgR1Quc3RyaW5nKSkpKVxuOztcbiJdfQ==