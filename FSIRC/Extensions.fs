module Extensions
    [<RequireQualifiedAccess>]
    module Result =
        let ofChoice choice =
            match choice with
            | Choice1Of2 a -> Ok a
            | Choice2Of2 a -> Error a

    [<RequireQualifiedAccess>]
    module Async =
        let awaitTaskResult (task : System.Threading.Tasks.Task) =
            task
            |> Async.AwaitTask
            |> Async.Catch
            |> Async.RunSynchronously
            |> Result.ofChoice