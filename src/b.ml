open A

module String_id = struct

  type t = string
  let of_string s = s
  let to_string s = s
    
end

module Username : ID = String_id
module Hostname : ID = String_id

type session_info = {
  user : Username.t;
  host : Hostname.t
}

let sessions_have_same_user s1 s2 =
  Username.to_string s1.user = Hostname.to_string s2.host
              
module B_module = struct
  type t = int
  let of_int i = i
end

module B : A_module_type = B_module
