// Generated by js_of_ocaml
//# buildInfo:effects=false, use-js-string=false, version=5.0.1
(function
   (globalThis)
   {"use strict";
    var runtime=globalThis.jsoo_runtime;
    function caml_call1(f,a0)
     {return f.length == 1?f(a0):runtime.caml_call_gen(f,[a0])}
    function caml_call2(f,a0,a1)
     {return f.length == 2?f(a0,a1):runtime.caml_call_gen(f,[a0,a1])}
    var
     global_data=runtime.caml_get_global_data(),
     cst_gettimeofday_failed=
      runtime.caml_string_of_jsbytes("gettimeofday failed"),
     Base_Int63=global_data.Base__Int63,
     Base=global_data.Base;
    function nanoseconds_since_unix_epoch(param)
     {var t=runtime.time_now_nanoseconds_since_unix_epoch_or_zero(0);
      return caml_call2(Base_Int63[17],t,Base_Int63[38])
              ?t
              :caml_call1(Base[200],cst_gettimeofday_failed)}
    var Time_now=[0,nanoseconds_since_unix_epoch];
    runtime.caml_register_global(3,Time_now,"Time_now");
    return}
  (globalThis));

//# sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLjAsImZpbGUiOiJ0aW1lX25vdy5jbWEuanMiLCJzb3VyY2VSb290IjoiIiwibmFtZXMiOlsibmFub3NlY29uZHNfc2luY2VfdW5peF9lcG9jaCIsInQiXSwic291cmNlcyI6WyIvVXNlcnMvYXphdC8ub3BhbS80LjE0LjAvbGliL3RpbWVfbm93L3RpbWVfbm93Lm1sIl0sIm1hcHBpbmdzIjoiOzs7OztJOzs7OztJQStCRTs7Ozs7O2FBREVBO01BQ00sSUFBSkMsRUFBSTtNQUNMLGlDQURDQTs7ZUFMeUIsNkNBTTZCO29CQUZ4REQ7O1UiLCJzb3VyY2VzQ29udGVudCI6WyJbJSVpbXBvcnQgXCJjb25maWcuaFwiXVxuXG5vcGVuISBCYXNlXG5cblslJWlmZGVmIEpTQ19BUkNIX1NJWFRZRk9VUl1cblxuZXh0ZXJuYWwgbmFub3NlY29uZHNfc2luY2VfdW5peF9lcG9jaF9vcl96ZXJvXG4gIDogIHVuaXRcbiAgLT4gSW50NjMudFxuICA9IFwidGltZV9ub3dfbmFub3NlY29uZHNfc2luY2VfdW5peF9lcG9jaF9vcl96ZXJvXCJcbltAQG5vYWxsb2NdXG5cblslJWVsc2VdXG5cbmV4dGVybmFsIG5hbm9zZWNvbmRzX3NpbmNlX3VuaXhfZXBvY2hfb3JfemVyb1xuICA6ICB1bml0XG4gIC0+IEludDYzLnRcbiAgPSBcInRpbWVfbm93X25hbm9zZWNvbmRzX3NpbmNlX3VuaXhfZXBvY2hfb3JfemVyb1wiXG5cblslJWVuZGlmXVxuWyUlaWZkZWYgSlNDX1BPU0lYX1RJTUVSU11cblxubGV0W0Bjb2xkXSBnZXR0aW1lX2ZhaWxlZCAoKSA9IGZhaWx3aXRoIFwiY2xvY2tfZ2V0dGltZShDTE9DS19SRUFMVElNRSkgZmFpbGVkXCJcblxuWyUlZWxzZV1cblxubGV0W0Bjb2xkXSBnZXR0aW1lX2ZhaWxlZCAoKSA9IGZhaWx3aXRoIFwiZ2V0dGltZW9mZGF5IGZhaWxlZFwiXG5cblslJWVuZGlmXVxuXG5sZXQgbmFub3NlY29uZHNfc2luY2VfdW5peF9lcG9jaCAoKSA9XG4gIGxldCB0ID0gbmFub3NlY29uZHNfc2luY2VfdW5peF9lcG9jaF9vcl96ZXJvICgpIGluXG4gIGlmIEludDYzLiggPD4gKSB0IEludDYzLnplcm8gdGhlbiB0IGVsc2UgZ2V0dGltZV9mYWlsZWQgKClcbjs7XG4iXX0=
