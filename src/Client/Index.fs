module Index

open Elmish
open Fable.Remoting.Client
open Shared
open System

type DescriptionInput = { Description: string; Touched: bool }
type DueDateInput = { DueDate: DateTime; Touched: bool }

type Model =
    { Todos: Todo list
      Description: DescriptionInput
      DueDate: DueDateInput }

type Msg =
    | GotTodos of Todo list
    | SetDescriptionInput of string
    | SetDueDateInput of DateTime
    | AddTodo
    | AddedTodo of Todo

type ValidationResult =
    | Valid of string
    | Invalid of string
    | NotTouched

let todosApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    let model =
        { Todos = []
          Description = { Description = ""; Touched = false }
          DueDate =
            { DueDate = DateTime.Today
              Touched = false } }

    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos

    model, cmd

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotTodos todos -> { model with Todos = todos |> Todo.sort }, Cmd.none
    | SetDescriptionInput value -> { model with Description = { Description = value; Touched = true } }, Cmd.none
    | SetDueDateInput value -> { model with DueDate = { DueDate = value; Touched = true } }, Cmd.none
    | AddTodo ->
        let todo = Todo.create model.Description.Description model.DueDate.DueDate

        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo

        { model with
            Description = { Description = ""; Touched = false }
            DueDate =
                { DueDate = DateTime.Today
                  Touched = false } },
        cmd
    | AddedTodo todo -> { model with Todos = model.Todos @ [ todo ] |> Todo.sort }, Cmd.none

open Feliz
open Feliz.Bulma

module Formatters =
    let formattedTodo (todo: Todo) =
        sprintf "%s - due: %s" todo.Description (Todo.shortDate todo)


module Handlers =
    let addTodoWhenValid (model: Model) (dispatch: Msg -> unit) =
        let isValid = Todo.isValid model.Description.Description model.DueDate.DueDate

        match isValid with
        | true -> AddTodo |> dispatch
        | false ->

            ()


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

    let todoContent (todo: Todo) =
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
                    prop.onClick (fun _ -> ())
                    prop.text "Delete"
                ]
            ]
        ]

    let todoRow (todo: Todo) (dispatch: Msg -> unit) =
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


    let dueDateTouched (model: Model) = model.DueDate.Touched

    let dueDateValid (model: Model) = Todo.dueDateValid model.DueDate.DueDate

    let descriptionTouched (model: Model) = model.Description.Touched

    let descriptionValid (model: Model) =
        Todo.descriptionValid model.Description.Description

    let getValidationResult (validMessage: string, invalidMessage: string) =
        function
        | true -> Valid validMessage
        | false -> Invalid invalidMessage

    let validateTouchedDueDate (model: Model) =
        model
        |> dueDateValid
        |> getValidationResult ("Due Date is valid", "Due Date must be today or later")

    let validateTouchedDescription (model: Model) =
        model
        |> descriptionValid
        |> getValidationResult ("Description is valid", "Description is required")

    let validateDueDate (model: Model) : ValidationResult =
        model
        |> dueDateTouched
        |> function
            | true -> validateTouchedDueDate model
            | false -> NotTouched

    let validateDescription (model: Model) : ValidationResult =
        model
        |> descriptionTouched
        |> function
            | true -> validateTouchedDescription model
            | false -> NotTouched

    let descriptionInput (model: Model) (dispatch: Msg -> unit) =
        Bulma.control.div [
            control.isExpanded
            prop.children [
                Bulma.input.text [
                    prop.value model.Description.Description
                    prop.placeholder "What needs to be done?"
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
                    Todo.isValid model.Description.Description model.DueDate.DueDate
                    |> not
                )
                prop.onClick (fun _ -> Handlers.addTodoWhenValid model dispatch)
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