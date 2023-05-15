module Shared.Tests

#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

open Shared
open System


module DateTimes =
    let todayOrLaterReturnsTrueForToday () =
        testCase "Today should be today or later."
        <| fun () ->
            let expected = true
            let actual = DateTimes.todayOrLater DateTime.Today
            Expect.equal actual expected "Should be true"

    let todayOrLaterReturnsTrueForTomorrow () =
        testCase "Tomorrow should be today or later."
        <| fun () ->
            let expected = true
            let actual = DateTimes.todayOrLater (DateTime.Today.AddDays 1.0)
            Expect.equal actual expected "Should be true"


    let todayOrLaterReturnsFalseForYesterday () =
        testCase "Yesterday should not be today or later."
        <| fun () ->
            let expected = false
            let actual = DateTimes.todayOrLater (DateTime.Today.AddDays -1.0)
            Expect.equal actual expected "Should be false"


module Strings =
    let notEmptyReturnsTrueForNonEmptyString () =
        testCase "Non-empty string should not be empty."
        <| fun () ->
            let expected = true
            let actual = Strings.notEmpty "test"
            Expect.equal actual expected "Should be true"

    let notEmptyReturnsFalseForEmptyString () =
        testCase "Empty string should be empty."
        <| fun () ->
            let expected = false
            let actual = Strings.notEmpty ""
            Expect.equal actual expected "Should be false"


    let notEmptyReturnsFalseForWhiteSpace () =
        testCase "Whitespace string should be empty."
        <| fun () ->
            let expected = false
            let actual = Strings.notEmpty " "
            Expect.equal actual expected "Should be false"

module Todos =
    let emptyDescriptionNotValid () =
        testCase "Empty string is not a valid description"
        <| fun () ->
            let expected = false
            let actual = Todo.descriptionValid ""
            Expect.equal actual expected "Should be false"

    let whiteSpaceDescriptionNotValid () =
        testCase "Whitespace string is not a valid description"
        <| fun () ->
            let expected = false
            let actual = Todo.descriptionValid " "
            Expect.equal actual expected "Should be false"

    let validDescriptionIsValid () =
        testCase "Non-empty string is a valid description"
        <| fun () ->
            let expected = true
            let actual = Todo.descriptionValid "test"
            Expect.equal actual expected "Should be true"

    let priorDateNotValidDueDate () =
        testCase "Date in the past is not a valid due date"
        <| fun () ->
            let expected = false
            let actual = Todo.dueDateValid (DateTime.Today.AddDays -1.0)
            Expect.equal actual expected "Should be false"

    let todayDateIsValidDueDate () =
        testCase "Today should be a valid due date"
        <| fun () ->
            let expected = true
            let actual = Todo.dueDateValid DateTime.Today
            Expect.equal actual expected "Should be true"

    let tomorrowIsValidDueDate () =
        testCase "Tomorrow should be a valid due date"
        <| fun () ->
            let expected = true
            let actual = Todo.dueDateValid (DateTime.Today.AddDays 1.0)
            Expect.equal actual expected "Should be true"

    let shortDateReturnsShortDateForTodoItem () =
        testCase "Short date should be returned for todo item"
        <| fun () ->
            let todo = Todo.create "test" (DateTime.Parse "2023-05-14")
            let expected = "05/14/2023"
            let actual = Todo.shortDate todo
            Expect.equal actual expected "Dates should match"

    let sortTodosShouldBeByDate () =
        testCase "Todos should be sorted by date"
        <| fun () ->
            let todo1 = Todo.create "test1" (DateTime.Parse "2023-05-14")
            let todo2 = Todo.create "test2" (DateTime.Parse "2023-05-15")
            let todo3 = Todo.create "test3" (DateTime.Parse "2023-05-16")
            let todos = [ todo2; todo3; todo1 ]
            let expected = [ todo1; todo2; todo3 ]
            let actual = Todo.sort todos
            Expect.equal actual expected "Should be true"

    let createShouldMakeNewTodos () =
        testCase "Create should make new todos"
        <| fun () ->
            let id = Guid.NewGuid()
            let description = "test"
            let dueDate = (DateTime.Parse "2023-05-14")

            let expected =
                { Id = id
                  Description = description
                  DueDate = dueDate }

            let actual = Todo.create description dueDate
            Expect.equal actual.Description expected.Description "Descriptions should be equal"
            Expect.equal actual.DueDate expected.DueDate "Due dates should be equal"
            Expect.notEqual actual.Id expected.Id "Ids should not be equal"


let sharedTodo =
    testList
        "Shared.Todo"
        [ Todos.emptyDescriptionNotValid ()
          Todos.whiteSpaceDescriptionNotValid ()
          Todos.priorDateNotValidDueDate ()
          Todos.validDescriptionIsValid ()
          Todos.todayDateIsValidDueDate ()
          Todos.tomorrowIsValidDueDate ()
          Todos.shortDateReturnsShortDateForTodoItem ()
          Todos.sortTodosShouldBeByDate ()
          Todos.createShouldMakeNewTodos () ]

let sharedDateTimes =
    testList
        "Shared.DateTimes"
        [ DateTimes.todayOrLaterReturnsTrueForToday ()
          DateTimes.todayOrLaterReturnsTrueForTomorrow ()
          DateTimes.todayOrLaterReturnsFalseForYesterday () ]

let sharedStrings =
    testList
        "Shared.Strings"
        [ Strings.notEmptyReturnsTrueForNonEmptyString ()
          Strings.notEmptyReturnsFalseForEmptyString ()
          Strings.notEmptyReturnsFalseForWhiteSpace () ]