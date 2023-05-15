module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared
open System

module Mappings =
    let toValidateModel (todo: TodoItem) =
        todo.Description, todo.DueDate

module Storage =
    let todos = ResizeArray<TodoItem>()

    let addTodo (todo: TodoItem) =
        todo
        |> Mappings.toValidateModel
        |> Todo.isValid
        |> function
        | true ->
            todos.Add todo
            Ok()
        | false -> Error "Invalid todo"

module ApiFunctions =
    let addTodo (todo: TodoItem) =
        async {
            return
                match Storage.addTodo todo with
                | Ok () -> Some todo
                | Error _ -> None
        }

    let getTodos () =
        async { return Storage.todos |> List.ofSeq }

    let deleteTodo (id: Guid) =
        async {
            return
                Storage.todos
                |> Seq.tryFind (fun todo -> todo.Id = id)
                |> function
                | None -> id
                | Some todo ->
                    Storage.todos.Remove todo |> ignore
                    id
        }

let todosApi =
    { getTodos = ApiFunctions.getTodos
      addTodo = ApiFunctions.addTodo
      deleteTodo = ApiFunctions.deleteTodo }

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