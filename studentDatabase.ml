type student = {
  first_name : string;
  last_name : string;
  id : int;
  semester : int;
  grades : (int * float) list;
}
type database = student list

let  insert s db = s::db 

let rec find_by_id id db = match db with [] -> []
  | x::xs -> if x.id = id then [x] else find_by_id id xs 
 
let rec find_by_last_name name db = match db with [] -> []
  | x::xs -> if x.last_name = name
    then x::find_by_last_name name xs
    else find_by_last_name name xs;; 

let rec remove_by_id id db =
  match db with [] -> []
  | x::xs -> if x.id = id then xs else x::remove_by_id id xs


let rec count_in_semester sem db =
  match db with [] -> 0
  | x::xs -> (if x.semester = sem then 1 else 0) + count_in_semester sem xs


let student_avg_grade id db =
  let rec list_avg sum n l =
    match l with [] -> sum /. n
    | (_, grade)::xs -> list_avg (sum +. grade) (n +. 1.0) xs
  in
  match find_by_id id db with
  | [{ grades=[] }] -> 0.0
  | [s] -> list_avg    0.0     0.0     s.grades
  | _ -> 0.0


  let course_avg_grade course db =
  let rec iter_grades (sum, n) l =
    match l with [] -> sum, n
    | (c,g)::xs -> if c = course then iter_grades (sum +. g, n +. 1.0) xs else iter_grades (sum, n) xs
  in
  let rec iter_students (sum, n) l =
    match l with [] -> (sum, n)
    | x::xs -> iter_students (iter_grades (sum, n) x.grades) xs
  in
  let sum, n = iter_students (0.0, 0.0) db in
  if n = 0.0 then 0.0 else sum /. n


