type context = { branch : Zwmlib.Store.Backend.t option; uuid : string option }
val live : ?context:context -> Dream.websocket -> unit Lwt.t
