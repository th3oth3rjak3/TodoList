namespace Shared

open System

type TodoItem =
    { Id: Guid
      Description: string
      DueDate: DateTime }

module Strings =
    let notEmpty (value: string) = String.IsNullOrWhiteSpace value |> not

module DateTimes =
    let todayOrLater (date: DateTime) = date >= DateTime.Today

module Todo =

    let descriptionValid (description: string) = description |> Strings.notEmpty

    let dueDateValid (dueDate: DateTime) = dueDate |> DateTimes.todayOrLater

    let isValid (description: string) (dueDate: DateTime) =
        descriptionValid description
        && dueDateValid dueDate

    let create (description: string) (dueDate: DateTime) =
        { Id = Guid.NewGuid()
          Description = description
          DueDate = dueDate }

    let shortDate (todo: TodoItem) = todo.DueDate.ToString("MM/dd/yyyy")

    let sort (todos: TodoItem list) =
        List.sortBy (fun todo -> todo.DueDate) todos

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<TodoItem list>
      addTodo: TodoItem -> Async<TodoItem>
      deleteTodo: Guid -> Async<Guid>}