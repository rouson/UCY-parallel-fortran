UCY-parallel-fortran
====================
Code jams and homework exercises for Parallel Programming in Modern Fortran course at the University of Cyprus

Homework 1
----------
In this assignment, you start writing the world's longest Hello world program with modern Fortran features:
* Blocks
* Deferred-lenth character variables (`character(len=:), allocatable :: foo`)
* Collective subroutine:
```fortran
call co_broadcast(input,source_image=1)
```

### To Do
- [ ] Write the my_message() function
- [ ] Go out for frappe


