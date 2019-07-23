module type HttpBody = {
  type t;
  type io(+'a);

  let to_string: t => io(string);
  let of_string: string => t;
};

module Make =
       (
         Schema: Graphql_intf.Schema,
         Body: HttpBody with type io(+'a) := Schema.Io.t('a),
       ) => {
  module Io = Schema.Io;

  let (>>=) = Io.bind;

  type callback('conn) =
    ('conn, Cohttp.Request.t, Body.t) =>
    Schema.Io.t((Cohttp.Response.t, Body.t));

  let addCORS = (response: Cohttp.Response.t): Cohttp.Response.t => {
    let headers = Cohttp.Response.headers(response);
    let newHeaders =
      Cohttp.Header.add_list(
        headers,
        [
          ("access-control-allow-origin", "*"),
          ("access-control-allow-headers", "Accept, Content-Type"),
          (
            "access-control-allow-methods",
            "GET, HEAD, POST, DELETE, OPTIONS, PUT, PATCH",
          ),
        ],
      );

    let newResponse =
      Fieldslib.Field.fset(
        Cohttp.Response.Fields.headers,
        response,
        newHeaders,
      );
    newResponse;
  };

  let respond_string = (~status, ~body, ()) => {
    let response = Cohttp.Response.make(~status, ()) |> addCORS;
    Io.return((response, Body.of_string(body)));
  };

  let static_file_response = path =>
    switch (Assets.read(path)) {
    | Some(body) => respond_string(~status=`OK, ~body, ())
    | None => respond_string(~status=`Not_found, ~body="", ())
    };

  let json_err =
    fun
    | Ok(_) as ok => ok
    | Error(err) => Error(`String(err));

  let execute_query = (ctx, schema, variables, operation_name, query) => {
    let parser_result = json_err(Graphql_parser.parse(query));
    Io.return(parser_result)
    >>= (
      fun
      | Ok(doc) =>
        Schema.execute(schema, ctx, ~variables?, ~operation_name?, doc)
      | Error(_) as e => Io.return(e)
    );
  };

  let execute_request = (schema, ctx, _req, body) =>
    Body.to_string(body)
    >>= (
      body' => {
        let json = Yojson.Basic.from_string(body');
        let query =
          Yojson.Basic.(json |> Util.member("query") |> Util.to_string);
        let variables =
          Yojson.Basic.Util.(
            json |> member("variables") |> to_option(to_assoc)
          );
        let operation_name =
          Yojson.Basic.Util.(
            json |> member("operationName") |> to_option(to_string)
          );
        let result =
          execute_query(
            ctx,
            schema,
            (
              variables :> option(list((string, Graphql_parser.const_value)))
            ),
            operation_name,
            query,
          );
        result
        >>= (
          fun
          | Ok(`Response(data)) => {
              let body = Yojson.Basic.to_string(data);
              respond_string(~status=`OK, ~body, ());
            }
          | Ok(`Stream(stream)) => {
              Schema.Io.Stream.close(stream);
              let body = "Subscriptions are only supported via websocket transport";
              respond_string(~status=`Internal_server_error, ~body, ());
            }
          | Error(err) => {
              let body = Yojson.Basic.to_string(err);
              respond_string(~status=`Internal_server_error, ~body, ());
            }
        );
      }
    );

  let serve_from_public_dir = path =>
    respond_string(~status=`OK, ~body=path, ());

  let make_callback =
      (make_context, schema, _conn, req: Cohttp.Request.t, body) => {
    let req_path = Cohttp.Request.uri(req) |> Uri.path;
    let path_parts = Str.(split(regexp("/"), req_path));
    switch (req.meth, path_parts) {
    | (`GET, ["graphql"]) => static_file_response("index.html")
    | (`GET, ["graphql", path]) => static_file_response(path)
    | (`GET, ["home"]) => serve_from_public_dir("/home")
    | (`GET, ["home", path]) => serve_from_public_dir(path)
    | (`GET, []) => serve_from_public_dir("/")
    | (`GET, [path]) => serve_from_public_dir(path)
    | (`POST, ["graphql"]) =>
      execute_request(schema, make_context(req), req, body)
    | (`OPTIONS, ["graphql"]) =>
      let response = Cohttp.Response.make(~status=`No_content, ());
      Io.return((addCORS(response), Body.of_string("")));
    | _ => respond_string(~status=`Not_found, ~body="", ())
    };
  };
};
