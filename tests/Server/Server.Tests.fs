module Server.Tests

open Expecto

open Shared
open Server
open System


let addValidTodoShouldSucceed () =
    testCase "Adding a valid Todo should succeed."
    <| fun () ->
        let validTodo = Todo.create "TODO" DateTime.Today
        let result = Storage.addTodo validTodo
        Expect.isOk result "Result should be ok"
        Expect.contains Storage.todos validTodo "Storage should contain new todo"

let addInvalidTodoShouldFail () =
    testCase "Adding an invalid Todo should fail."
    <| fun () ->
        let invalidTodo = Todo.create "" DateTime.Today
        let expectedResult = Error "Invalid todo"
        let result = Storage.addTodo invalidTodo
        Expect.equal result expectedResult "Result should be error"

        Storage.todos
        |> Seq.tryFind (fun todo -> todo = invalidTodo)
        |> fun item -> Expect.isNone item "Storage should not contain new todo"

let server =
    testList
        "Server"
        [ addValidTodoShouldSucceed ()
          addInvalidTodoShouldFail () ]

let all =
    testList
        "All"
        [ Tests.sharedTodo
          Tests.sharedDateTimes
          Tests.sharedStrings
          server ]

[<EntryPoint>]
let main _ = runTestsWithCLIArgs [] [||] all