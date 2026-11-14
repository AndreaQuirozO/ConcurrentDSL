a := create_lock();
t := spawn { u := spawn { lock a; unlock a }; join u };
lock a; unlock a;
join t
