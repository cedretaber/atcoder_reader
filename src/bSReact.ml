module RR = ReasonReact

module RD = ReactDOMRe

module RE = ReactEvent

external s : string -> RR.reactElement = "%identity"

let div ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "div" ~props @@ Array.of_list children

let span ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "span" ~props @@ Array.of_list children

let h1 ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "h3" ~props @@ Array.of_list children

let h3 ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "h3" ~props @@ Array.of_list children

let input ?class_name ?type_ ?value ?on_change ?on_click children =
  let props = RD.props
    ?className:class_name
    ?type_
    ?value
    ?onChange:on_change
    ?onClick:on_click
    () in
  RD.createElementVariadic "input" ~props @@ Array.of_list children

let table ?class_name children =
  let props = RD.props
    ?className:class_name
    () in
  RD.createElementVariadic "table" ~props @@ Array.of_list children

let thead children =
  let props = RD.props () in
  RD.createElementVariadic "thead" ~props @@ Array.of_list children

let tbody children =
  let props = RD.props () in
  RD.createElementVariadic "tbody" ~props @@ Array.of_list children

let th ?colspan children =
  let props = RD.props
    ?colSpan:colspan
    () in
  RD.createElementVariadic "th" ~props @@ Array.of_list children

let tr children =
  let props = RD.props () in
  RD.createElementVariadic "tr" ~props @@ Array.of_list children

let td ?colspan children =
  let props = RD.props
    ?colSpan:colspan
    () in
  RD.createElementVariadic "td" ~props @@ Array.of_list children