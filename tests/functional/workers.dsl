x := 0; a := create_lock();
t1 := spawn { i := 0; while i < 3 do { lock a; x := x + 1; unlock a; i := i + 1 } };
t2 := spawn { i := 0; while i < 3 do { lock a; x := x + 1; unlock a; i := i + 1 } };
join t1; join t2
