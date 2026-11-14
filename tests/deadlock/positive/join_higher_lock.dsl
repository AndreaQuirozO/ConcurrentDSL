a := create_lock(); b := create_lock();
lock a;
t := spawn { lock b; unlock b };
join t;
unlock a
