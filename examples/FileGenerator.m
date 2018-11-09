#!/usr/local/bin/MathematicaScript -script

<<RandFile`

(* set directory for stroing output files *)
currentDir = "/home/user/RandFile_Examples";
SetDirectory[currentDir];

(* define the used bias *)
a[7] = 2/10;
a[k_] := Module[{i}, Sum[1/2^(i - 1), {i, 1, k - 1}]/10];

(* some basic quality testing *)
qualityFun = {Mean[#] // N, Entropy[2, #] // N} &;

(* sample size *)
mb = 1; (* basic unit in KB (1KB = 1024 bytes) *)
noMB = 1; (* number of the base units *)
noBits = 8*1024*mb*noMB; (* actual number of required bits *)

randomData = "sampledata-100MB.bin"; (* must contain at least noBits bits *)
baseName = "mySample"; (* output to baseName1.bin, baseName2.bin etc *)

(* get number of basic units *)
If[Length[$ScriptCommandLine] > 1,
	noSamples = ToExpression[$ScriptCommandLine[[2]]],
	noSamples = 1
	];

(* produce sample using TrueRandomChoice *)
Table[
	SetTrueRandomDataFile[randomData];
	GetTrueRadnomDataFile[];
	Print[RandFile`Private`TrueRandomDataFile];
	theSample[i] = TrueRandomChoice[{0.3 + a[i], 0.7 - a[i]} -> {0, 1}, noBits];
	CloseTrueRandomDataFile[];
	Export[baseName <> ToString[i] <> ".bin", theSample[i], "Bit"],
	{i, noSamples}
];

samplesQuality = Map[Flatten[qualityFun /@ {theSample[#]}] &, Range[noSamples]];

Print[samplesQuality];

Export[baseName <> "-quality.dat",samplesQuality]; 
