
type tree = Node of int * tree * tree | Empty

let rec to_list t = match t with Empty -> []
    | Node (v, l, r) -> (to_list l) @ (v :: to_list r)

let rec insert x t = match t with Empty -> Node (x, Empty, Empty)
    | Node (v, l, r) -> if x < v then Node (v, insert x l, r) 
                      else if x > v then Node (v, l, insert x r)
                      else t


                      let rec remove_max t = match t with Empty -> failwith "unreachable"
                      | Node (v, l, Empty) -> v, l
                      | Node (v, l, r) -> let v',r' = remove_max r in v',Node (v, l, r')
                      
                  let rec remove x t = match t with Empty -> Empty
                      | Node (v, l, r) -> if x < v then Node (v, remove x l, r)
                                        else if x > v then Node (v, l, remove x r)
                                        else if l = Empty then r else 
                                            let v',l' = remove_max l in Node (v', l', r)
