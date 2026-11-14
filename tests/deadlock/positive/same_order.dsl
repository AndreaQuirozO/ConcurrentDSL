a := create_lock(); b := create_lock();
t := spawn { lock a; lock b; unlock b; unlock a };
lock a; lock b; unlock b; unlock a;
join t
