type result_type
  = AC
  | WA
  | TLE
  | MLE
  | CE
  | Other of string

let type_of_string = function
    "AC" -> AC
  | "WA" -> WA
  | "TLE" -> TLE
  | "MLE" -> MLE
  | "CE" -> CE
  | other -> Other other

type t = {
  id: int;
  user_id: string;
  contest_id: string;
  problem_id: string;
  result: result_type;
  point: float;
  epoch_second: int;
  execution_time: int;
  language: string;
  length: int
}