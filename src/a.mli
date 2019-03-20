module type ID = sig

  type t
  val of_string : string -> t
  val to_string : t -> string
    
end



module type A_module_type = sig
  type t
  val of_int : int -> t
    
end
