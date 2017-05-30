UCY-parallel-fortran
====================
Code jams and homework exercises for Parallel Programming in Modern Fortran course at the University of Cyprus

1. [Homework 1](#homework-1)
2. [Homework 2](#homework-2)
2. [Homework 3](#homework-3)

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

Homework 2
----------
In this assignment, you will use the co_sum collective subroutine to sum integers across all images.
### To Do
- [ ] Fork this repository by clicking "Fork" in the upper right corner of the browser page (don't click the number next to "Fork")
- [ ] Clone your fork and install the sourcery library:
```bash
  git clone https:github.com/<your-github-username>/UCY-parallel-fortran
  cd UCY-parallel-fortran/src/homework2/library
  make install
```
- [ ] Complete the steps listed after the text "Assignment" in the file src/homework2/procedural_fireworks/main.f90
- [ ] With your working directory set to `src/homework2/procedural_fireworks`, build and run the application using the `make` utility:
```bash
  make 
  cafrun -n 4 ./procedural_fireworks < fireworks_input.txt
```
which executes `procedural_fireworks` in 4 images and redirects the standard input to the file `fireworks_intput.txt`.
- [ ] Bring your instructor a frappe to receive an A for the day.

Homework 3
----------
- [ ] Copy the [procedural fireworks solution](src/solutions/homework2/procedural_fireworks) into a new homework3 subdirectory inside [src](src).
- [ ] Eliminate the `result_image` argument in the copied main program's `co_sum` call.
- [ ] Refactor the new homework3 main program to replace `co_sum` with a call to a `co_sum_binary` that you will write.
- [ ] Write `co_sum_binary` as an internal subprogram after the `contains` statement in the new `main` program.  Your `co_sum_binary` will first calcuate the sum and store it on image 1 using a communication pattern that is essentially the reverse of the pattern in yesterday's [co_broadcast_binary code jam](src/code-jams/fortran-2008-test-my-message.f90).
- [ ] Because the `result_image` argument is not in the new `main` program's `co_sum` call, the resulting sum must be on each images after that image finishes executing `co_sum`.  For this purpose, the last step in your `co_sum_binary` should be to invoke `co_broadcast_binary` from the code jam.  Think carefully about whether any synchronizations are required at the beginning, middle, or end of your `co_sum_binary`.
