module Index

open Elmish
open Fable.Remoting.Client
open Shared
open System

type DescriptionInput = { Description: string; Touched: bool }
type DueDateInput = { DueDate: DateTime; Touched: bool }

type Model =
    { Todos: TodoItem list
      Description: DescriptionInput
      DueDate: DueDateInput }

type Msg =
    | GotTodos of TodoItem list
    | SetDescriptionInput of string
    | SetDueDateInput of DateTime
    | AddTodo
    | TriedAddTodo of TodoItem option
    | DeleteTodo of Guid
    | DeletedTodo of Guid

type ValidationResult =
    | Valid of string
    | Invalid of string
    | NotTouched

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>


let createDefaultDescription () : DescriptionInput = { Description = ""; Touched = false }

let createDefaultDueDate () : DueDateInput =
    { DueDate = DateTime.Today
      Touched = false }

let createDefaultModel () : Model =
    { Todos = []
      Description = createDefaultDescription ()
      DueDate = createDefaultDueDate () }

let init () : Model * Cmd<Msg> =
    createDefaultModel (), Cmd.OfAsync.perform todosApi.getTodos () GotTodos

module Mappings =
    let fromModelToTodo (model: Model) : TodoItem =
        Todo.create model.Description.Description model.DueDate.DueDate

    let toValidateModel (model: Model) =
        model.Description.Description, model.DueDate.DueDate

    let todosToModel (model: Model) (todos: TodoItem list) : Model = { model with Todos = todos }

    let resetModelWithTodos (model: Model) : Model =
        { model with
            Description = createDefaultDescription ()
            DueDate = createDefaultDueDate () }

module UpdateCommands =
    let updateGotTodos (todos: TodoItem list) (model: Model) =
        todos
        |> Todo.sort
        |> fun sortedTodos -> { model with Todos = sortedTodos }, Cmd.none

    let updateDescriptionInput (value: string) (model: Model) =
        { model with Description = { Description = value; Touched = true } }, Cmd.none

    let updateDueDateInput (value: DateTime) (model: Model) =
        { model with DueDate = { DueDate = value; Touched = true } }, Cmd.none

    let performTodoApiCmd (todo: TodoItem) =
        Cmd.OfAsync.perform todosApi.addTodo todo TriedAddTodo

    let addNewTodo (model: Model) =
        model
        |> Mappings.fromModelToTodo
        |> performTodoApiCmd
        |> fun cmd -> model, cmd

    let addedNewTodo (todo: TodoItem) (model: Model) =
        model.Todos @ [ todo ]
        |> Todo.sort
        |> Mappings.todosToModel model
        |> Mappings.resetModelWithTodos,
        Cmd.none

    let triedAddNewTodo (todo: TodoItem option) (model: Model) =
        match todo with
        | Some todo -> addedNewTodo todo model
        | None -> model, Cmd.none

    let deletedTodo (model: Model) (id: Guid) =
        model.Todos
        |> List.filter (fun todo -> todo.Id <> id)
        |> Mappings.todosToModel model,
        Cmd.none

    let performDeleteTodoCmd (id: Guid) =
        Cmd.OfAsync.perform todosApi.deleteTodo id DeletedTodo

    let deleteTodo (id: Guid) (model: Model) = model, performDeleteTodoCmd id


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> UpdateCommands.updateGotTodos todos model
    | SetDescriptionInput value -> UpdateCommands.updateDescriptionInput value model
    | SetDueDateInput value -> UpdateCommands.updateDueDateInput value model
    | TriedAddTodo optionTodo -> UpdateCommands.triedAddNewTodo optionTodo model
    | AddTodo -> UpdateCommands.addNewTodo model
    | DeletedTodo id -> UpdateCommands.deletedTodo model id
    | DeleteTodo id -> UpdateCommands.deleteTodo id model


open Feliz
open Feliz.Bulma

module Formatters =
    let formattedTodo (todo: TodoItem) =
        sprintf "%s - due: %s" todo.Description (Todo.shortDate todo)


module Handlers =

    let handleAddTodoDispatch (dispatch: Msg -> unit) =
        function
        | true -> AddTodo |> dispatch
        | false -> ()

    let addTodoIfValid (model: Model) (dispatch: Msg -> unit) =
        model
        |> Mappings.toValidateModel
        |> Todo.isValid
        |> handleAddTodoDispatch dispatch

module Validation =
    let dueDateTouched (model: Model) = model.DueDate.Touched

    let dueDateValid (model: Model) = Todo.dueDateValid model.DueDate.DueDate

    let descriptionTouched (model: Model) = model.Description.Touched

    let descriptionValid (model: Model) =
        Todo.descriptionValid model.Description.Description


module Components =
    let navBrand =
        Bulma.navbarBrand.div [
            Bulma.navbarItem.a [
                prop.href "https://safe-stack.github.io/"
                navbarItem.isActive
                prop.children [
                    Html.img [
                        prop.src "/favicon.png"
                        prop.alt "Logo"
                    ]
                ]
            ]
        ]

    let todoContent (todo: TodoItem) =
        Bulma.column [
            prop.children [
                Bulma.field.p [
                    prop.style [ style.color "white" ]
                    prop.text (Formatters.formattedTodo todo)
                ]
            ]
        ]

    let deleteButton (id: Guid) (dispatch: Msg -> unit) =
        Bulma.column [
            column.isNarrow
            prop.children [
                Bulma.button.a [
                    button.isSmall
                    color.isDanger
                    prop.onClick (fun _ -> DeleteTodo id |> dispatch)
                    prop.text "Delete"
                ]
            ]
        ]

    let todoRow (todo: TodoItem) (dispatch: Msg -> unit) =
        Bulma.columns [
            columns.isVCentered
            prop.children [
                deleteButton todo.Id dispatch
                todoContent todo
            ]
        ]

    let validationMessage (validationResult: ValidationResult) =
        match validationResult with
        | Valid message ->
            Bulma.field.p [
                prop.className "help is-success"
                prop.text message
            ]
        | Invalid message ->
            Bulma.field.p [
                prop.className "help is-danger"
                prop.text message
            ]
        | NotTouched ->
            Bulma.field.p [
                prop.className "help is-success"
                prop.text ""
            ]


    let getValidationResult (validMessage: string, invalidMessage: string) =
        function
        | true -> Valid validMessage
        | false -> Invalid invalidMessage

    let validateTouchedDueDate (model: Model) =
        model
        |> Validation.dueDateValid
        |> getValidationResult ("Due Date is valid", "Due Date must be today or later")

    let validateTouchedDescription (model: Model) =
        model
        |> Validation.descriptionValid
        |> getValidationResult ("Description is valid", "Description is required")

    let validateDueDate (model: Model) : ValidationResult =
        model
        |> Validation.dueDateTouched
        |> function
            | true -> validateTouchedDueDate model
            | false -> NotTouched

    let validateDescription (model: Model) : ValidationResult =
        model
        |> Validation.descriptionTouched
        |> function
            | true -> validateTouchedDescription model
            | false -> NotTouched

    let descriptionInput (model: Model) (dispatch: Msg -> unit) =
        Bulma.control.div [
            control.isExpanded
            prop.children [
                Bulma.input.text [
                    prop.value model.Description.Description
                    prop.placeholder "What to do?"
                    prop.onChange (fun x -> SetDescriptionInput x |> dispatch)
                ]
                model |> validateDescription |> validationMessage
            ]
        ]

    let dueDateInput (model: Model) (dispatch: Msg -> unit) =
        Bulma.control.div [
            control.isExpanded
            prop.children [
                Bulma.input.date [
                    prop.value model.DueDate.DueDate
                    prop.onChange (fun x -> SetDueDateInput x |> dispatch)
                ]
                model |> validateDueDate |> validationMessage
            ]
        ]

    let addButton (model: Model) (dispatch: Msg -> unit) =
        Bulma.control.p [
            Bulma.button.a [
                color.isPrimary
                prop.disabled (
                    model
                    |> Mappings.toValidateModel
                    |> Todo.isValid
                    |> not
                )
                prop.onClick (fun _ -> Handlers.addTodoIfValid model dispatch)
                prop.text "Add"
            ]
        ]


    let inputRow (model: Model) (dispatch: Msg -> unit) =
        Bulma.field.div [
            field.isGrouped
            prop.children [
                descriptionInput model dispatch
                dueDateInput model dispatch
                addButton model dispatch
            ]
        ]

    let todoListTitle =
        Bulma.title [
            prop.style [ style.color "white" ]
            text.hasTextCentered
            prop.text "To-Do List"
        ]

    let todoBox (model: Model) (dispatch: Msg -> unit) =
        Bulma.box [
            prop.style [
                style.backgroundColor "#262626"
            ]
            prop.children [
                todoListTitle
                for todo in model.Todos do
                    todoRow todo dispatch
                inputRow model dispatch
            ]
        ]



let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.style [
            style.backgroundSize "cover"
            style.backgroundImageUrl "https://unsplash.it/1200/900?random"
            style.backgroundPosition "no-repeat center center fixed"
        ]
        prop.children [
            Bulma.heroHead [
                Bulma.navbar [
                    Bulma.container [ Components.navBrand ]
                ]
            ]
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is8
                        column.isOffset2
                        prop.children [
                            Components.todoBox model dispatch
                        ]
                    ]
                ]
            ]
        ]
    ]