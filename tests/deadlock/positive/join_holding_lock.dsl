a := create_lock();
x := 1;
t := spawn {
    x := x + 1
};
lock a;
join t;
unlock a