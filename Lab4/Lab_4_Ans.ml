type grade = A|B|C|D|F;;

let score_to_grade (s:int) : grade = 
  if s>=80 then A
    else if s>=60 then B
  else if s>=40 then C
    else if s>=20 then D
      else F;;

type gd_rcd ={
  name : string;
  score :int;
  lt_gd : grade;
};;

type gd_sheet ={
    course_code:string;
    instructor:string;
    content : gd_rcd list;
};;

let rec grade_check (gd_rcd_list:gd_rcd list) = 
  match gd_rcd_list with
  |[]-> false
  |[x] -> x.lt_gd == score_to_grade x.score
  |hd::tl -> (hd.lt_gd == score_to_grade hd.score) && grade_check tl;;
