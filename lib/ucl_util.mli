
(** Protected input channel that is closed anyway *)
val with_input_channel : in_channel -> (in_channel -> 'a) -> 'a