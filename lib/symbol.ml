
type typ = TInt | TVoid

type var_info = {
  v_type : typ;
  initialized : bool;
}

type func_info = {
  ret_type : typ;
  params : (typ * string) list;
}
