include Stdlib.Result
type ('a, 'b) result = ('a, 'b) Stdlib.Result.t = Ok of 'a | Error of 'b
