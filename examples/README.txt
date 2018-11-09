This directory contains examples of the usage for the RandFile package.
Notebooks RandFile_ValueOfPi.nb and RandFile_VisualDSE.nb were developed by
R.Heinen in collaboration PicoQuant and modified by J.A. Miszczak in order to
accept different source of random data. 

- README.txt - this file,
- RandFile_FileGenerator.m - Mathematica script allowing to generate biased
  samples used in the experiments - requires a file with (true) random data,
- RandFile_ValueOfPi.nb - Monte Carlo calculation of pi with various plots
  illustrating influence of the bias on the results,
- piSample{1..7}.bin - samples used for calculating value of pi as describe in
  paper arXiv:1302.2738,
- RandFile_VisualDSE.nb - double split experiment implemented using file-based
  sources of randomness,
- dseSample{1..7}.bin - sample used to executing double slit experiment with
  biased data as described in paper arXiv:1302.2738.

Examples in this directory have been tested with version 0.0.17 of RandFile
package. Recent version of the package and the above files can be obtained from
http://www.iitis.pl/~miszczak/rand_file/

Experiments described in paper arXiv:1302.2738 were obtained using:
- RandFile_ValueOfPi.nb using samples piSample{1..7}.bin
- RandFile_VisualDSE.nb using samples dseSample{1..7}.bin

New samples can be generated using RandFile_FileGenerator.m and a file with 
true random data.
