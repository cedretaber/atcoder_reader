let maybe_user = match [%bs.raw {|(location.search.match(/[?&]user=(\w+)/) || [0,""])[1]|}] with
    "" -> None
  | user_id -> Some user_id

let _ = ReactDOMRe.renderToElementWithId (App.e ?user_id:maybe_user []) "app"