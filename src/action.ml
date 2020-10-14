type t = { label:string; step:int}
let compare l1 l2 = Int.compare l1.step l2.step
let make l s = {label=l; step=s}