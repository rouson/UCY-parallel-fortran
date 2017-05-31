UCY-parallel-fortran
====================
Code jams and homework exercises for Parallel Programming in Modern Fortran course at the University of Cyprus

1. [Homework 1](#homework-1)
2. [Homework 2](#homework-2)
3. [Homework 3](#homework-3)
4. [Homework 4](#homework-4)

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
- [ ] Generate a pull request on your repository and merge the latest central repository into yours [central repository](https://github.com/rouson/UCY-parallel-fortran). (Alternatively, you could clone the latest central repository, but you don't have write permissions on it so you won't be able to push your changes, which loses you some GitHub functionality that is only available online.)
- [ ] Try building the updated/cloned repository with CMake, e.g.
```bash
cd UCY-parallel-fortran-<insert-github-user-name>
mkdir build
cd build
FC=caf cmake ..
make
```
- [ ] The resulting build failure will point you to the new Homework 3 [main](src/homework3/fortran2008_procedural_fireworks) program, which is identical to the homework 2 `main` except that the `co_sum_binary` replaces `co_sum` and the corresponding `result_image` argument has been eliminarted.
- [ ] Follow the instructions below the text "Assignment" in the new [main] program to complete Homework 3.  A completed assignment will cause all tests to pass when you run `ctest` after the above `make`

Homework 4
----------
- [ ] Build the course archive with CMake as usual:
```bash
cd UCY-parallel-fortran
mkdir build
cd build
FC=caf cmake ..
make
```
- [ ] The build fails because it's looking for a missing file: synchronous-derivative-put.f90.  Create the missing file by copying [synchronous-derivative-get.f90](src/homework4/synchronous-derivative-get.f90)
- [ ] Inside the new file, define an "inbox" coarray to hold incoming boundary data from neighboring images in the executing image's halo.
- [ ] Replace the "gets" in the `get_halo` block with "puts" to ensure the initiation of communication as soon as the communicated data is ready.
- [ ] Impose any necessary synchronizations via `sync images`.
- [ ] Continue building and testing until issuing the command `ctest` reports that all tests pass.
