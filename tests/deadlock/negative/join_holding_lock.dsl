a := create_lock();
t := spawn { lock a; unlock a };
lock a; join t; unlock a
