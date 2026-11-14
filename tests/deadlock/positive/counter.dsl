x := 0; a := create_lock();
t := spawn { lock a; x := x + 1; unlock a };
lock a; x := x + 10; unlock a;
join t
