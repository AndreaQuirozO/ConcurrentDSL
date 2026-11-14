a := create_lock(); lock a;
t := spawn { lock a; unlock a };
join t; unlock a
