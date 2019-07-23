open Graphql_lwt;

type query = {
  id: option(string),
  string: option(string),
  listOfStrings: option(list(option(string))),
  listOfObjects: option(list(option(query))),
  listOfInterfaces: option(list(option(query))),
};

let queryMock = {
  id: Some("id"),
  string: Some("Hello World!"),
  listOfStrings: Some(Utils.initList(100, Some("Hello World!"))),
  listOfObjects: None,
  listOfInterfaces: None,
};

let schema =
  Schema.(
    schema([
      io_field("id", ~args=Arg.[], ~typ=string, ~resolve=(info, ()) =>
        Ok(queryMock.id) |> Lwt.return
      ),
      io_field("string", ~args=[], ~typ=string, ~resolve=(info, ()) =>
        Ok(queryMock.string) |> Lwt.return
      ),
      io_field(
        "listOfStrings", ~args=[], ~typ=list(string), ~resolve=(info, ()) =>
        Ok(queryMock.listOfStrings) |> Lwt.return
      ),
    ])
  );

let start_server = () => {
  /* Generate my own Cohttp Server */
  module Graphql_cohttp_lwt = CohttpServer.Make(Schema, Cohttp_lwt.Body);

  let callback =
    Graphql_cohttp_lwt.make_callback(
      req => req |> Cohttp.Request.headers |> ignore,
      schema,
    );
  let server = Cohttp_lwt_unix.Server.make(~callback, ());
  let mode = `TCP(`Port(8080));
  print_endline("Hi");
  Cohttp_lwt_unix.Server.create(~mode, server);
};
