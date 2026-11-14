a := create_lock(); b := create_lock();
t := spawn { lock a; lock b; unlock b; unlock a };
lock b; lock a; unlock a; unlock b;
join t
