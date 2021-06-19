let _clamp bottom top i =
  let i = min i (top-1) in
  let i = max i bottom in
  i


type el = float

type t = {
  left_: el list;
  focus_: el;
  right_: el list;
  index_: int;
  length_: int;
}

let of_array arr ~index =
  let n = Array.length arr in
  let i = _clamp 0 n index in
  {
    left_=Array.sub arr 0 i
          |> Array.to_list
          |> List.rev;
    focus_=arr.(i);
    right_=Array.sub arr (i+1) (n-(i+1))
           |> Array.to_list;
    index_=i;
    length_=n;
  }

let to_array {left_; focus_; right_; _} =
  let left_ = left_
              |> List.rev
              |> Array.of_list in
  let right_ = right_
               |> Array.of_list in
  Array.concat [left_; [|focus_|]; right_]

let get t = t.focus_

let set {left_; focus_=_; right_; index_; length_} ~value =
  {
    left_;
    focus_=value;
    right_;
    index_;
    length_;
  }

let map ~f {left_; focus_; right_; index_; length_} =
  {
    left_=List.map f left_;
    focus_=f focus_;
    right_=List.map f right_;
    index_;
    length_;
  }

let shift_left t =
  match t.left_ with
  | [] -> t
  | new_focus_ :: new_left_ ->
    let new_right_ = t.focus_ :: t.right_ in
    {
      left_=new_left_;
      focus_=new_focus_;
      right_=new_right_;
      index_=(t.index_ - 1);
      length_=t.length_;
    }

let shift_right t =
  match t.right_ with
  | [] -> t
  | new_focus_ :: new_right_ ->
    let new_left_ = t.focus_ :: t.left_ in
    {
      left_=new_left_;
      focus_=new_focus_;
      right_=new_right_;
      index_=(t.index_ + 1);
      length_=t.length_;
    }

let rec jump_to t ~index =
  let n = t.length_ in
  let new_index = _clamp 0 n index in
  let curr_index = t.index_ in
  match compare curr_index new_index with
  | 0 -> t
  (* if were are currently at a higher index then need to move left *)
  | 1 -> shift_left t
         |> jump_to ~index:new_index
  (* if we are currently at a lower index then need to move right *)
  | _ -> shift_right t
         |> jump_to ~index:new_index
