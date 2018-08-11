open BSReact

module ResultType = Result

module Result = struct
  let component = RR.statelessComponent "Result"

  let elm_of_type = function
      ResultType.AC -> span ~class_name:"has-text-success" [ s "AC" ]
    | ResultType.WA -> span ~class_name:"has-text-danger" [ s "WA" ]
    | ResultType.TLE -> span ~class_name:"has-text-danger" [ s "TLE" ]
    | ResultType.MLE -> span ~class_name:"has-text-danger" [ s "MLE" ]
    | ResultType.CE -> span ~class_name:"has-text-warning" [ s "CE" ]
    | ResultType.Other str -> span [ s str ]

  let make ~result:{ResultType.problem_id; result; point; epoch_second; execution_time; language} _children = {
    component with
    render= fun _self ->
      let datetime =
        let time_t = Js.Date.fromFloat @@ float_of_int epoch_second *. 1000. in
        let date = Js.Date.toLocaleDateString time_t in
        let time = Js.Date.toLocaleTimeString time_t in
        {j|$date $time|j} in
      tr [
        td [ s problem_id ];
        td [ elm_of_type result ];
        td [ s @@ string_of_int @@ int_of_float point ];
        td [ s datetime ];
        td [ s @@ string_of_int execution_time ];
        td [ s language ]
      ]
  }

  let e ~result children = RR.element @@ make ~result children
end

let result_set contest_id result_list =
  tr [
    th ~colspan:6 [
      s contest_id
    ]
  ] :: List.map (fun result -> Result.e ~result []) result_list

let component = RR.statelessComponent "ResultList"

let make ~result_list _children = {
  component with
  render= fun _self ->
    let module SMap = Map.Make(String) in
    let result_list =
      List.sort
        (fun {ResultType.epoch_second= e1} {epoch_second= e2} -> compare e2 e1)
        result_list in
    let result_map, contest_list =
      List.fold_right
        (fun ({ResultType.contest_id} as result) (acc, contests) ->
          if SMap.mem contest_id acc then
            let results = result :: SMap.find contest_id acc in
            let acc = SMap.add contest_id results acc in
            (acc, contests)
          else
            ((SMap.add contest_id [result] acc), contest_id :: contests))
        result_list
        (SMap.empty, []) in
    let result_list =
      List.fold_right
        (fun contest_id acm ->
          List.fold_right
            (fun result acm -> result :: acm)
            (result_set contest_id (SMap.find contest_id result_map))
            acm)
        contest_list
        [] in
    table ~class_name:"table" [
      thead [
        tr [
          td [];
          td [];
          td [];
          td [];
          td [];
          td []
        ]
      ];
      tbody result_list
    ]
}

let e ~result_list children = RR.element @@ make ~result_list children