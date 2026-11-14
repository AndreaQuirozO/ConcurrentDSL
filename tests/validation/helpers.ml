let failures = ref 0

let check name ok =
  if ok then Printf.printf "OK:   %s\n" name
  else (incr failures; Printf.printf "FAIL: %s\n" name)

let finish label =
  if !failures > 0 then (Printf.printf "\n%d %s test(s) FAILED\n" !failures label; exit 1)
  else Printf.printf "\nall %s tests PASSED\n" label;
  Printf.printf "-------------------"

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0