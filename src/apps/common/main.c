/* Matthew Hammer <hammer@ttic.edu> */

/* The main() function is apart of the CEAL program; this makes it
   easy for our CEAL compiler to insert extra initialization steps
   into the program, as it has access to the program entry point,
   namely, the main function.

   After this initialization code is run, main() delegates running the
   performance tests to some external controller (written in
   conventional C code and compiled seperately, e.g., by GCC).
*/

int cealtestperf_main(int argc, char** argv);

int main(int argc, char** argv) {
  return cealtestperf_main(argc, argv);
}
