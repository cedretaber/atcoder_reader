open! BSReact

type state = {
  user_id: string;
  result_list: Result.t list
}

type action
  = FetchResult
  | SuccessFetchResult of Js.Json.t
  | FailureFetchResult
  | ChangeUserId of string

let initial_state user_id () = {
  user_id= (match user_id with Some user_id -> user_id | None -> "");
  result_list= []
}

let result_of_json json = Js.Json.(match decodeObject json with
    None -> None
  | Some obj ->
    let get = Js.Dict.get obj in
    match
      get "id",
      get "user_id",
      get "contest_id",
      get "problem_id",
      get "result",
      get "point",
      get "epoch_second",
      get "execution_time",
      get "language",
      get "length"
    with
        Some id,
        Some user_id,
        Some contest_id,
        Some problem_id,
        Some result,
        Some point,
        Some epoch_second,
        Some execution_time,
        Some language,
        Some length -> (match
          decodeNumber id,
          decodeString user_id,
          decodeString contest_id,
          decodeString problem_id,
          decodeString result,
          decodeNumber point,
          decodeNumber epoch_second,
          decodeNumber execution_time,
          decodeString language,
          decodeNumber length
        with
            Some id,
            Some user_id,
            Some contest_id,
            Some problem_id,
            Some result,
            Some point,
            Some epoch_second,
            Some execution_time,
            Some language,
            Some length -> Some {
              Result.id= int_of_float id; user_id; contest_id; problem_id;
              result= Result.type_of_string result; point; epoch_second= int_of_float epoch_second; execution_time= int_of_float execution_time;
              language; length= int_of_float length
            }
          | _ -> None)
      | _ -> None)

let reducer action state = match action, state with
    FetchResult, {user_id} ->
    if user_id = "" then RR.NoUpdate
    else
      let url = {j|https://kenkoooo.com/atcoder/atcoder-api/results?user=$user_id|j} in
      RR.SideEffects Js.Promise.(fun self ->
        let _ =
          Fetch.fetch url
          |> then_ Fetch.Response.json
          |> then_ (fun json ->
            Js.log json;
            resolve @@ self.send (SuccessFetchResult json))
          |> catch (fun err ->
            Js.log err;
            resolve @@ self.send FailureFetchResult) in
        ())
  | SuccessFetchResult json, _ ->
    Js.Json.(
      let result_array = match decodeArray json with
          Some arr -> arr
        | None -> [||] in
      let result_list =
        Array.fold_right (fun json acc -> match result_of_json json with
            Some result -> result :: acc
          | None -> acc)
          result_array
          [] in
      RR.Update { state with result_list }
    )
  | FailureFetchResult, _ ->
    RR.NoUpdate
  | ChangeUserId user_id, _ ->
    RR.Update { state with user_id }

let didMount self =
  self.RR.send FetchResult

let change_username self event =
  let user_id = (RE.Form.target event)##value in
  self.RR.send (ChangeUserId user_id)

let click_submit self event =
  RE.Mouse.preventDefault event;
  self.RR.send FetchResult

let component = RR.reducerComponent "App"

let make ?user_id _children = {
  component with
  initialState= initial_state user_id;
  reducer;
  didMount;
  render= fun self ->
    let {user_id; result_list} = self.state in
    div ~class_name:"atcoder-reader-main container" [
      h1 ~class_name:"title is-1" [
        s "ATCODER READER"
      ];
      div ~class_name:"field has-addons" [
        div ~class_name:"control" [
          input ~class_name:"input" ~type_:"text" ~on_change:(change_username self) ~value:user_id []
        ];
        div ~class_name:"control" [
          input ~class_name:"input" ~type_:"submit" ~on_click:(click_submit self) ~value:{j|取得|j} []
        ]
      ];
      ResultList.e ~result_list []
    ]
}

let e ?user_id children = RR.element @@ make ?user_id children