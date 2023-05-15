module Client.Tests

open Fable.Mocha

open Index
open Shared
open System


module Commands =
    let addTodo () =
        testCase "AddedTodo should add a todo item."
        <| fun () ->
            let newTodo = Todo.create "new todo" DateTime.Today
            let model, _ = init ()
            let model, _ = update (AddedTodo newTodo) model
            Expect.equal model.Todos.Length 1 "There should be 1 todo"
            Expect.equal model.Todos.[0] newTodo "Todo should equal new todo"

    let addTodoWithExistingModel () =
        testCase "AddTodo should reset the model."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDescriptionInput "new todo") model
            let model, _ = update (SetDueDateInput DateTime.Today) model
            let model, _ = update AddTodo model
            Expect.equal model.Description.Description "" "Description should be empty"
            Expect.equal model.DueDate.DueDate DateTime.Today "DueDate should be today"
            Expect.equal model.Description.Touched false "Description should not be touched"
            Expect.equal model.Description.Touched false "DueDate should not be touched"


    let gotTodos () =
        testCase "GotTodos should set the todos to an empty list."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (GotTodos []) model
            Expect.equal model.Todos.Length 0 "There should be 0 todos"

module Components =
    let dueDateTouched () =
        testCase "DueDate should be touched when SetDueDateInput is called."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDueDateInput DateTime.Today) model
            Expect.equal model.DueDate.Touched true "DueDate should be touched"



module Validation =
    let dueDateValidWhenToday () =
        testCase "DueDate should be valid when the model contains today's date."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDueDateInput DateTime.Today) model
            let result = model |> Validation.dueDateValid
            Expect.equal result true "DueDate should be valid"

    let dueDateValidWhenTomorrow () =
        testCase "DueDate should be valid when the model contains tomorrow's date."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDueDateInput(DateTime.Today.AddDays 1.0)) model
            let result = model |> Validation.dueDateValid
            Expect.equal result true "DueDate should be valid"

    let dueDateInvalidWhenYesterday () =
        testCase "DueDate should not be valid when in the past."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDueDateInput(DateTime.Today.AddDays -1.0)) model
            let result = model |> Validation.dueDateValid
            Expect.equal result false "DueDate should not be valid"

    let dueDateNotTouchedOnInit () =
        testCase "DueDate should not be touched after initialization."
        <| fun () ->
            let model, _ = init ()
            let result = model |> Validation.dueDateTouched
            Expect.equal result false "DueDate should not be touched"

    let dueDateTouchedAfterUpdate () =
        testCase "DueDate should be touched after update called."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDueDateInput DateTime.Today) model
            let result = model |> Validation.dueDateTouched
            Expect.equal result true "DueDate should be touched"

    let descriptionInvalidWhenEmpty () =
        testCase "Description should be invalid when empty."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDescriptionInput "") model
            let result = model |> Validation.descriptionValid
            Expect.equal result false "Description should be invalid"

    let descriptionInvalidWhenWhiteSpace () =
        testCase "Description should be invalid when whitespace."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDescriptionInput " ") model
            let result = model |> Validation.descriptionValid
            Expect.equal result false "Description should be invalid"

    let descriptionShouldBeValidWhenNonEmpty () =
        testCase "Description should be valid when non-empty."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDescriptionInput "a") model
            let result = model |> Validation.descriptionValid
            Expect.equal result true "Description should be valid"

    let descriptionShouldNotBeTouchedOnInit () =
        testCase "Description should not be touched on init."
        <| fun () ->
            let model, _ = init ()
            let result = model |> Validation.descriptionTouched
            Expect.equal result false "Description should not be touched."

    let descriptionShouldBeTouchedAfterUpdate () =
        testCase "Description should be touched after update."
        <| fun () ->
            let model, _ = init ()
            let model, _ = update (SetDescriptionInput "a") model
            let result = model |> Validation.descriptionTouched
            Expect.equal result true "Description should be touched."



let clientCommands =
    testList
        "Client"
        [ Commands.addTodo ()
          Commands.addTodoWithExistingModel ()
          Commands.gotTodos () ]

let clientFunctions = testList "Client.Components" [ Components.dueDateTouched () ]

let clientValidation =
    testList
        "Client.Validation"
        [ Validation.dueDateValidWhenToday ()
          Validation.dueDateValidWhenTomorrow ()
          Validation.dueDateInvalidWhenYesterday ()
          Validation.dueDateNotTouchedOnInit ()
          Validation.dueDateTouchedAfterUpdate ()
          Validation.descriptionInvalidWhenEmpty ()
          Validation.descriptionInvalidWhenWhiteSpace ()
          Validation.descriptionShouldBeValidWhenNonEmpty ()
          Validation.descriptionShouldNotBeTouchedOnInit ()
          Validation.descriptionShouldBeTouchedAfterUpdate () ]

let all =
    testList
        "All"
        [
#if FABLE_COMPILER // This preprocessor directive makes editor happy
          Shared.Tests.sharedDateTimes
          Shared.Tests.sharedTodo
          Shared.Tests.sharedStrings
#endif
          clientCommands
          clientFunctions
          clientValidation ]

[<EntryPoint>]
let main _ = Mocha.runTests all