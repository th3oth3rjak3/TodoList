module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Storage =
    let todos = ResizeArray()

    let addTodo (todo: TodoItem) =
        let { Description = description
              DueDate = dueDate } =
            todo

        match Todo.isValid description dueDate with
        | true ->
            todos.Add todo
            Ok()
        | false -> Error "Invalid todo"

module ApiFunctions =
    let addTodo (todo: TodoItem) =
        async {
            return
                match Storage.addTodo todo with
                | Ok () -> todo
                | Error e -> failwith e
        }

    let getTodos () =
        async { return Storage.todos |> List.ofSeq }

let todosApi =
    { getTodos = ApiFunctions.getTodos
      addTodo = ApiFunctions.addTodo }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

[<EntryPoint>]
let main _ =
    run app
    0