(* ::Package:: *)

(* ::Section:: *)
(*Header*)


(* File: RandFile.m *)
(* Description: Mathematica package for generating random numbers from random data stored in files. *)
(* Authors: Jaroslaw Miszczak <miszczak@iitis.pl> *)
(* License: GPLv2 *)


BeginPackage["RandFile`"];


Unprotect@@Names["RandFile`*"];


Clear@@Names["RandFile`*" ];


(* ::Subtitle:: *)
(*Public part*)


(* ::Section:: *)
(*Manipulating the source of random data*)


SetTrueRandomDataFile::usage = "SetTrueRandomDataFile[fName] sets the global variable TrueRandomDataFile used by the functions for generating random numbers. For example SetTrueRandomDataFile[\"/home/user/data/random_file.bin\"].";


GetTrueRandomDataFile::usage = "GetTrueRandomDataFile[] displays the value of the global variable TrueRandomDataFile.";


CloseTrueRandomDataFile::usage = "CloseTrueRandomDataFile[] closes the file assigned by the global variable set using SetTrueRandomDataFile.";


TrueRandomDataFile::usage = "TrueRandomDataFile is the global variable, defined in RandFile`Private context, storing a string with the name of the file containing random data.";


TrueRandomDataFileBytesCount::usage = "TrueRandomDataFileBytesCount is the global variable for storing a number of bytes in the TrueRandomDataFile.";


BlockTrueRandom::usage = "BlockTrueRandom[ex] evaluates expression ex and stores the starting position in the file RandFile`Private`TrueRandomDataFile. The position in the file is restored after the execution. Note that this function works only with expressions using the global variable for pointing to a file with random data. See also BlockRandom built-in Mathematica function.";


TrueRandomSequence::usage = "TrueRandomSequence[] changes the current position in the file assigned by global variable TrueRandomDataFile so that the numbers generated after the execution of this function will not overlap with the previously generated numbers.\n";


TrueRandomSequence::usage = TrueRandomSequence::usage <> "TrueRandomSequence[pos] marks the current position in the file with random data or, if the position pos is already marked, returns to this position in the file.";


TrueRandomDataMarkers::usage = "TrueRandomDataMarkers is a global variable for accumulating positions in the file with random data used by TrueRandomSequence function. Note that this array is sorted with respect to the second argument.";


SetMaxTrueRandomSequenceLength::usage = "SetMaxTrueRandomSequenceLength[len] declares the maximal length, expressed in bytes, of the true random sequence used during the session.";


GetMaxTrueRandomSequenceLength::usage = "GetMaxTrueRandomSequenceLength[] displays the maximal length of the random sequence which can be used during the session.";


GetTrueRandomDataMarkers::usage = "GetTrueRandomDataMarkers[] displays the currently set markers in the data files that are used by TrueRandomDataSequence.";


(* ::Section:: *)
(*Random integers*)


TrueRandomInteger::usage = "TrueRandomInteger[] produces an integer number in [0,1].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[n] produces an integer number in [0,n].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[n,k] produces k integer numbers in [0,n].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[{a,b}] produces an integer number in [a,b).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[{a,b}, k] produces k integer numbers in [a,b).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[{a,b}, {d1,d2,...,dk}] produces a {d1,d2,...,dk}-dimensional array of integer numbers in [a,b).\n\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[inStream] uses bytes from input stream inStream to produce a integer number in [0,1).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[n, inStream] uses bytes from input stream inStream to produce a integer number in [0,n).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[n, k, inStream] uses bytes from input stream inStream to produce k integer numbers in [0,n).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[{a,b}, inStream] uses bytes from input stream inStream to produce an integer number in [a,b).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[{a,b}, k, inStream] uses bytes from input stream inStream to produce k integer numbers in [a,b).\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[{a,b}, {d1,d2,...,dk}, inStream] uses bytes from input stream inStream to produce a {d1,d2,...,dk}-dimensional array of integer numbers in [a,b).\n\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[PoissonDistribution[\[Mu]]] produces random integer from PoissonDistribution[\[Mu]].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[PoissonDistribution[\[Mu]], k] produces k random integers from PoissonDistribution[\[Mu]].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[PoissonDistribution[\[Mu]], {d1,d2,...,dk}] produces a {d1,d2,...,dk}-dimensional array of integer numbers from PoissonDistribution[\[Mu]].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[PoissonDistribution[\[Mu]], inStream] uses bytes from input stream inStream to produce a random integer from PoissonDistribution[\[Mu]].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[PoissonDistribution[\[Mu]], k, inStream] uses bytes from input stream inStream to produce k random integers from PoissonDistribution[\[Mu]].\n";


TrueRandomInteger::usage = TrueRandomInteger::usage <> "TrueRandomInteger[PoissonDistribution[\[Mu]], {d1,d2,...,dk}, inStream] uses bytes from input stream inStream to produce ka {d1,d2,...,dk}-dimensional array of integer numbers from PoissonDistribution[\[Mu]].";


(* ::Section:: *)
(*Random reals*)


TrueRandomReal::usage = "TrueRandomReal[] produces 32-bit real number in [0,1].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[b] produces 32-bit real number in [0,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[{a,b}] produces 32-bit real number in [a,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[b, n] produces n 32-bit real numbers in [0,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[{a,b}, n] produces n 32-bit real numbers in [a,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[b, {d1,d2,...,dk}] produces a {d1,d2,...,dk}-dimensional array of 32-bit real numbers in [0,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[{a,b}, {d1,d2,...,dk}] produces a {d1,d2,...,dk}-dimensional array of 32-bit real numbers in [a,b].\n\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[inStream] uses bytes from the input stream inStream to produce 32-bit real number in [0,1].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[b, inStream] uses bytes from the input stream inStream to produce 32-bit real number in [0,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[b, n, inStream] uses bytes from the input stream inStream to produce to produce n 32-bit real numbers in [0,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[{a,b}, inStream] uses bytes from the input stream inStream to produce 32-bit real number in [a,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[{a,b}, n, inStream] uses bytes from the input stream inStream to produce n 32-bit real numbers in [a,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[b, {d1,d2,...,dk}, inStream] uses bytes from the input stream inStream to produce a {d1,d2,...,dk}-dimensional array of 32-bit real numbers in [0,b].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[{a,b}, {d1,d2,...,dk}, inStream] uses bytes from the input stream inStream to produce a {d1,d2,...,dk}-dimensional array of 32-bit real numbers in [a,b].\n\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[NormalDistribution[\[Mu],\[Sigma]]] produces 32-bit real number from NormalDistribution[\[Mu],\[Sigma]].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[NormalDistribution[\[Mu],\[Sigma]], n] produces n 32-bit real numbers from NormalDistribution[\[Mu],\[Sigma]].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[NormalDistribution[\[Mu],\[Sigma]], {d1,d2,...,dk}] produces a {d1,d2,...,dk}-dimensional array of 32-bit real numbers from NormalDistribution[\[Mu],\[Sigma]].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[NormalDistribution[\[Mu],\[Sigma]], inStream] uses bytes from the input stream inStream to produce 32-bit real number from NormalDistribution[\[Mu],\[Sigma]].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[NormalDistribution[\[Mu],\[Sigma]], n, inStream] uses bytes from the input stream inStream to produce n 32-bit real numbers from NormalDistribution[\[Mu],\[Sigma]].\n";


TrueRandomReal::usage = TrueRandomReal::usage <> "TrueRandomReal[NormalDistribution[\[Mu],\[Sigma]], {d1,d2,...,dk}, inStream] uses bytes from the input stream inStream to produce a {d1,d2,...,dk}-dimensional array of 32-bit real numbers from NormalDistribution[\[Mu],\[Sigma]].";


(* ::Section:: *)
(*Random complex*)


TrueRandomComplex::usage = "TrueRandomComplex[] gives a true random complex number with real and imaginary parts in the range 0 to 1.\n";


TrueRandomComplex::usage = TrueRandomComplex::usage <> "TrueRandomComplex[{min,max}] gives a true random complex number in the rectangle with corners given by the complex numbers min and max.\n";


TrueRandomComplex::usage = TrueRandomComplex::usage <> "TrueRandomComplex[{min,max}, n] gives a list of d true random complex numbers in a given rectangle.\n";


TrueRandomComplex::usage = TrueRandomComplex::usage <> "TrueRandomComplex[{min,max}, {d1,d2,...,dk}] gives a {d1,d2,...,dk}-simensional array of true random complex numbers in a given rectangle.";


(* ::Section::Closed:: *)
(*Deprecated Random[] interface*)


TrueRandom::usage = "TrueRandom[type,range] gives a true random number of type Real, Integer or Complex in a specified range. See built-in Mathematica function Random.";


(* ::Section::Closed:: *)
(*TrueRandomChoice*)


TrueRandomChoice::usage = "TrueRandomChoice[{e1,e2,...,ek}] gives a true random choice of one of {e1,e2,...,en}.\n";


TrueRandomChoice::usage = TrueRandomChoice::usage <> "TrueRandomChoice[{e1,e2,...,ek}, n] gives a list of n true random choices.\n";


TrueRandomChoice::usage = TrueRandomChoice::usage <> "TrueRandomChoice[elist, {d1,d2,...,dk}] gives a {d1,d2,...,dk}-dimensional array of true random choices.\n";


TrueRandomChoice::usage = TrueRandomChoice::usage <> "TrueRandomChoice[{w1,w2,...,wn} -> {e1,e2,...,en}] gives a true random choice weighted by {w1,w2,...,wn}.\n";


TrueRandomChoice::usage = TrueRandomChoice::usage <> "TrueRandomChoice[{w1,w2,...,wn} -> {e1,e2,...,en}, k] gives k true random choices weighted by {w1,w2,...,wn}.\n";


TrueRandomChoice::usage = TrueRandomChoice::usage <> "TrueRandomChoice[{w1,w2,...,wn} -> {e1,e2,...,en}, {d1,d2,...,dk}] gives a {d1,d2,...,dk}-dimensional array of true random choices weighted by {w1,w2,...,wn}.";


(* ::Section::Closed:: *)
(*TrueRandomSample*)


TrueRandomSample::usage = "TrueRandomSample[l] gives a true random permutation of the list l.\n";


TrueRandomSample::usage = TrueRandomSample::usage <> "TrueRandomSample[l,n] gives n elements from the true random sample of the list l. Note that it is possible to take at most Length[l] elements.\n";


TrueRandomSample::usage = TrueRandomSample::usage <> "TrueRandomSample[{w1,w2,...,wn} -> {e1,e2,...,en}, k] gives k elements from the non-uniform sample of elements {e1,e2,...,en} with weights {w1,w2,...,wn}.";


(* ::Subtitle:: *)
(*Private part*)


Begin["`Private`"];


(* ::Section:: *)
(*Change log, version information*)


randfileHistory = {
	{"0.0.1", "09/10/2012", "Jarek", "Initial version with basic implementation of TrueRandomReal and TrueRandomInteger."},
	{"0.0.2", "18/10/2012", "Jarek", "Tests with functions using global variable."},
	{"0.0.3", "18/10/2012", "Jarek", "More error checking."},
	{"0.0.4", "26/10/2012", "Jarek", "Two patterns for reading random reals using global variable, functions for 64-bit real numbers."},
	{"0.0.5", "06/11/2012", "Jarek", "BlockTrueRandom, spelling improvements."},
	{"0.0.6", "21/11/2012", "Jarek", "Basic versions of TrueRandom and TrueRandomChoice, private function TrueRandomChoiceIntervals."},
	{"0.0.7", "22/11/2012", "Jarek", "More patterns for TrueRandomChoice, TrueRandomInteger, TrueRandomReal and TrueRandomDouble, private functions CallAsTableVersion and CallAsArrayVersion."},
	{"0.0.8", "26/11/2012", "Jarek", "Poisson distribution and normal distribution."},
	{"0.0.9", "03/12/2012", "Jarek", "TrueRandomSample and TrueRandomComplex, preliminary version of TrueRandomSequence, spelling improvements."},
	{"0.0.10", "04/12/2012", "Jarek", "Fixed pattern maching in TrueRandomInteger, changes in TrueRandomSequence, fixed SetTrueRandomDataFile."},
	{"0.0.11", "05/12/2012", "Jarek", "Major changes in TrueRandomSequence, added TrueRandomDataMarkers global variable, improved SetTrueRandomDataFile."},
	{"0.0.12", "14/12/2012", "Jarek", "Allocation and checking mechanism for random sequences."},
	{"0.0.13", "16/01/2013", "Jarek", "Documentation improvements."},
	{"0.0.14", "20/01/2013", "Jarek", "One new pattern for integers and reals, documentation improvements."},
	{"0.0.15", "21/01/2013", "Jarek", "Fixed functions for producing complex numbers."},
	{"0.0.16", "23/01/2013", "Jarek", "Removed TrueRandomDouble functions."},
	{"0.0.17", "25/01/2013", "Jarek", "Major documentation improvements, TrueRandomChoice fixed."},
	{"0.0.18", "24/06/2014", "Jarek", "Documentation improvements."},
	{"0.0.19", "09/03/2015", "Jarek", "TrueRandomInteger and documentation improvements."},
	{"0.0.20", "19/08/2015", "Jarek", "Tests with improved methods for reading data."},
	{"0.1.0", "20/08/2015", "Jarek", "Rewritten main input stream management, efficiency improvements."},
	{"0.1.1", "21/08/2015", "Jarek", "Code cleanups, documentation improvements. CallAsArrayVersion and CallAsTableVersion functions removed."},
	{"0.1.2", "25/08/2015", "Jarek", "Improvements in functions generating integers and in the documentation."},
	{"0.1.3", "26/08/2915", "Jarek", "Fixed problems with lists in the returned values."}
};


randfileVersion = Last[randfileHistory][[1]];


randfileLastModification = Last[randfileHistory][[2]];


(* ::Section::Closed:: *)
(*Error messages*)


RandFileNoMoreDataError::argerr = "EndOfFile occured in `1`! I'm unable to provide more random data.";


RandFileNoSuchFileError::argerr = "File \"`1`\" not found! Please make sure that the provided path is correct.";


RandFileWrongFormatError::argerr = "I can't read from \"`1`\"! It should be a binary file.";


RandFileStreamAlreadyOpen::fileerr = "File `1` is already opened for reading. I won't open new stream for this file.";


RandFileIntegerRangeError::argerr = "Provided range [`1`, `2`) is too wide! I can provide integers within at most 128 bit range.";


RandFileUnknownTypeError::arrerr = "I can't handel data of type \"`1`\"! Please use \"Integer\", \"Real\" or \"Complex\" instead.";


SetMaxTrueRandomSequenceLength::lenerr = "I can't allocate `1` in file \"`2`\" (containing `3` bytes). Please change the requested length - it should be non-zero.";


SetMaxTrueRandomSequenceLength::deferr = "Value of MaxTrueRandomSequenceLength variable cannot be redefined! You need to restart Mathematica kernel in order to define new value of this variable.";


TrueRandomSequence::markererr = "Unexpected problem with function TrueRandomSequence. Please contact the developer of this package.";


TrueRandomSequence::allocerr = "I'm unable to allocate sequence with label `1`, as it requires a file with more than `2` bytes (used file have only `3` bytes).";


TrueRandomSequence::rangeoverlap = "Requested usage of data from the next sequence. This is not allowed, as the two different sequences cannot overlap.";


(* ::Section::Closed:: *)
(*Manipulating the source of random data*)


SetTrueRandomDataFile[fileName_String]:=Block[{findFile=FindFile[fileName]}, 
	If[findFile==ToString[$Failed],
		Message[RandFileNoSuchFileError::argerr,findFile],
		(*else*)
		If[FileFormat[findFile]=="Binary",		
			RandFile`Private`TrueRandomDataFile = findFile;
			RandFile`Private`TrueRandomDataFileBytesCount = FileByteCount[findFile];
			If[Length[Streams[RandFile`Private`TrueRandomDataFile]]==1,
				Message[RandFileStreamAlreadyOpen::fileerr,findFile],
				RandFile`Private`TrueRandomDataStream = OpenRead[RandFile`Private`TrueRandomDataFile,BinaryFormat->True];
				Print["Using file \"" <> fileName <> "\" which contains " <> ToString[RandFile`Private`TrueRandomDataFileBytesCount] <> " bytes of data."];
			]
			,
			(*else*)
			Message[RandFileWrongFormatError::argerr,findFile]
		]
	]
];


GetTrueRandomDataFile[]:=If[ValueQ[RandFile`Private`TrueRandomDataFile],Print[RandFile`Private`TrueRandomDataFile]];


CloseTrueRandomDataFile[]:=Close[RandFile`Private`TrueRandomDataStream];


BlockTrueRandom[f_]:=Module[{startPos,result},
	startPos=StreamPosition[RandFile`Private`TrueRandomDataStream];
	result=f;
	SetStreamPosition[RandFile`Private`TrueRandomDataStream,startPos];
	result
];


SetAttributes[BlockTrueRandom,HoldAll];


SetMaxTrueRandomSequenceLength[len_Integer]:=Block[{},
	If[ValueQ[RandFile`Private`MaxTrueRandomSequenceLength],
		Message[SetMaxTrueRandomSequenceLength::deferr],
		If[0<len<=RandFile`Private`TrueRandomDataFileBytesCount,
			RandFile`Private`MaxTrueRandomSequenceLength = len;,
			Message[SetMaxTrueRandomSequenceLength::lenerr,len,RandFile`Private`TrueRandomDataFile,RandFile`Private`TrueRandomDataFileBytesCount]		
		];
	];
];


GetMaxTrueRandomSequenceLength[] = RandFile`Private`MaxTrueRandomSequenceLength;


GetTrueRandomDataMarkers[] = RandFile`Private`TrueRandomDataMarkers;


TrueRandomSequence[pos_Integer]:=Block[{posMarkers,allMarkers},
	posMarkers=Cases[RandFile`Private`TrueRandomDataMarkers,{pos,_}];
	allMarkers=Cases[RandFile`Private`TrueRandomDataMarkers,{_,_}];
	Switch[Length[posMarkers],
		0, (* no markers at this position yet *)
			If[Length[allMarkers]==0, (* this is the first one marker to set *)
				RandFile`Private`TrueRandomDataCurrentMarker={pos,0};
				AppendTo[RandFile`Private`TrueRandomDataMarkers,RandFile`Private`TrueRandomDataCurrentMarker];
				Sort[RandFile`Private`TrueRandomDataMarkers,#1[[2]]<#2[[2]]&];
				SetStreamPosition[RandFile`Private`TrueRandomDataFile,(RandFile`Private`TrueRandomDataCurrentMarker)[[2]]];
				(* there are alreay some markers so we need to allocate this one after the one with the largets position *),
				If[Max[allMarkers\[Transpose][[2]]]+RandFile`Private`MaxTrueRandomSequenceLength<RandFile`Private`TrueRandomDataFileBytesCount,
					RandFile`Private`TrueRandomDataCurrentMarker={pos,Max[allMarkers\[Transpose][[2]]]+RandFile`Private`MaxTrueRandomSequenceLength};					
					AppendTo[RandFile`Private`TrueRandomDataMarkers,RandFile`Private`TrueRandomDataCurrentMarker];
					Sort[RandFile`Private`TrueRandomDataMarkers,#1[[2]]<#2[[2]]&];
					SetStreamPosition[RandFile`Private`TrueRandomDataFile,(RandFile`Private`TrueRandomDataCurrentMarker)[[2]]];				
					,
					Message[TrueRandomSequence::allocerr,pos,Max[allMarkers\[Transpose][[2]]]+RandFile`Private`MaxTrueRandomSequenceLength,RandFile`Private`TrueRandomDataFileBytesCount];
				];
			];,
		1,
			RandFile`Private`TrueRandomDataCurrentMarker = posMarkers[[1]];
			SetStreamPosition[RandFile`Private`TrueRandomDataFile,(RandFile`Private`TrueRandomDataCurrentMarker)[[2]]];,
		_,
			Message[TrueRandomSequence::markererr];
	];
];


TrueRandomSequence[]:=Block[{pseudoRandLabel},
	pseudoRandLabel = RandomInteger[{-RandFile`Private`TrueRandomDataFileBytesCount,-1}];
	(* make sure that the label is not used *)
	While[Count[RandFile`Private`TrueRandomDataMarkers\[Transpose][[1]],pseudoRandLabel]!=0,
		pseudoRandLabel = RandomInteger[{-RandFile`Private`TrueRandomDataFileBytesCount,-1}];
	];
	TrueRandomSequence[pseudoRandLabel];
];


RandFile`Private`TrueRandomDataMarkers = {};


(*RandFile`Private`TrueRandomDataStream = $Failed;*)


(* ::Section:: *)
(*Random integers*)


TrueRandomInteger[inStream_InputStream]:=TrueRandomInteger[{0,1},1,inStream][[1]];


TrueRandomInteger[n_,inStream_InputStream]:=TrueRandomInteger[{0,n},1,inStream][[1]];


TrueRandomInteger[n_,k_,inStream_InputStream]:=TrueRandomInteger[{0,n},k,inStream];


TrueRandomInteger[{min_,max_},inStream_InputStream]:=TrueRandomInteger[{min,max},1,inStream][[1]];


TrueRandomInteger[{min_,max_},k_,inStream_InputStream]:=Block[{range,bitRange,byteRange,maxRange,rndLimit,readForm,rawRndIntList,rawRndInt,rndIntList,rndInt,curSeqPos},
	range=Abs[max-min]+1;
	bitRange=Ceiling[Log2[range]];
	byteRange=IntegerPart[(bitRange-1)/8]+1;
	rndIntList = {};
	
	If[Length[RandFile`Private`TrueRandomDataMarkers]>0,
		curSeqPos = Position[RandFile`Private`TrueRandomDataMarkers,RandFile`Private`TrueRandomDataCurrentMarker][[1,1]];
	];

	(* assign range and form used in BinaryRead and for scaling *)
	Which[ 
		bitRange<=8, readForm="UnsignedInteger8";maxRange=2^8,
		8<bitRange<=16, readForm="UnsignedInteger16";maxRange=2^16,
		16<bitRange<=24, readForm="UnsignedInteger24";maxRange=2^24,
		24<bitRange<=32, readForm="UnsignedInteger24";maxRange=2^32,
		32<bitRange<=64, readForm="UnsignedInteger24";maxRange=2^64,
		64<bitRange<=128, readForm="UnsignedInteger24";maxRange=2^128,
		_,Message[RandFileIntegerRangeError::argerr, min, max];
	];

	(*TODO: add check if we even use TrueRandomDataMarkers table *)
	(*SUGGESTION: Evaluation pattern similar to CallAsTable etc. but controlled with a global variable or a length of andFile`Private`TrueRandomDataMarkers array *)
	If[Length[RandFile`Private`TrueRandomDataMarkers]==0 || StreamPosition[RandFile`Private`TrueRandomDataFile]+ k byteRange <= RandFile`Private`TrueRandomDataMarkers[[curSeqPos,2]]+RandFile`Private`MaxTrueRandomSequenceLength,
		(* there is enough data in the current chunk *)	
		(* use only raw numbers from [0, rndLimit-1] *)
		rndLimit=maxRange-Mod[maxRange,range]; 

		(* read one raw integer number *)
		rawRndIntList=BinaryReadList[inStream,readForm,k];

		Do[
			Switch[rawRndInt,
				_Integer (* there are still data to read *),
					(* scale the raw number *)
					AppendTo[rndIntList,Mod[rawRndInt,range]+min],
				EndOfFile,	(* no more data to read - return Null *)
					Message[RandFileNoMoreDataError::argerr,inStream];
					Null
			]
		,{rawRndInt,rawRndIntList}
		],
		(* else for checking if the sequence is OK starts here *)
		Message[TrueRandomSequence::rangeoverlap];
	];
	rndIntList
];
	


TrueRandomInteger[{min_,max_},dims_List,inStream_InputStream]:=ArrayReshape[TrueRandomInteger[{min,max},Times@@dims,inStream],dims];


(* functions without inStream argument (assuming global input stream) *)


TrueRandomInteger[]:=TrueRandomInteger[{0,1},1,RandFile`Private`TrueRandomDataStream][[1]];


TrueRandomInteger[max_Integer]:=TrueRandomInteger[{1,max},RandFile`Private`TrueRandomDataStream];


TrueRandomInteger[{min_,max_}]:=TrueRandomInteger[{min,max},1,RandFile`Private`TrueRandomDataStream][[1]];


TrueRandomInteger[max_Integer, k_Integer]:=TrueRandomInteger[{1,max},k,RandFile`Private`TrueRandomDataStream];


TrueRandomInteger[{min_,max_}, k_Integer]:=TrueRandomInteger[{min,max},k,RandFile`Private`TrueRandomDataStream];


TrueRandomInteger[max_, dims_List]:=ArrayReshape[TrueRandomInteger[{1,max},Times@@dims,RandFile`Private`TrueRandomDataStream],dims];


TrueRandomInteger[{min_,max_}, dims_List]:=ArrayReshape[TrueRandomInteger[{min,max},Times@@dims,RandFile`Private`TrueRandomDataStream],dims];


(* additional functions for the Poisson distribution *)


TrueRandomInteger[dist_PoissonDistribution, inStream_InputStream]:=TrueRandomInteger[dist,1,inStream][[1]];


TrueRandomInteger[dist_PoissonDistribution, k_Integer, inStream_InputStream]:=Map[InverseCDF[dist,#]&,TrueRandomReal[{0,1},k,inStream]];


TrueRandomInteger[dist_PoissonDistribution, dims_,inStream_InputStream]:=ArrayReshape[TrueRandomInteger[dist,Times@@dims,inStream],dims];


TrueRandomInteger[dist_PoissonDistribution]:=TrueRandomInteger[dist,1,RandFile`Private`TrueRandomDataStream][[1]];


TrueRandomInteger[dist_PoissonDistribution, k_Integer]:=TrueRandomInteger[dist,k,RandFile`Private`TrueRandomDataStream];


TrueRandomInteger[dist_PoissonDistribution, dims_List]:=TrueRandomInteger[dist,dims,RandFile`Private`TrueRandomDataStream];


(* ::Section:: *)
(*Random reals*)


(* functions with inStream argument *)


TrueRandomReal[inStream_InputStream]:=TrueRandomReal[1,1,inStream][[1]];


TrueRandomReal[b_,inStream_InputStream]:=TrueRandomReal[b,1,inStream][[1]];


TrueRandomReal[{a_,b_},inStream_InputStream]:=TrueRandomReal[{a,b},1,inStream][[1]];


TrueRandomReal[b_,k_,inStream_InputStream]:=Block[{rawRndIntList,rawRndInt,tmpReal,realList,bits,byteRange,curSeqPos},

	byteRange=4;
	realList={};
	
	If[Length[RandFile`Private`TrueRandomDataMarkers]>0,
		curSeqPos = Position[RandFile`Private`TrueRandomDataMarkers,RandFile`Private`TrueRandomDataCurrentMarker][[1,1]];
	];
	
	If[Length[RandFile`Private`TrueRandomDataMarkers]==0 || StreamPosition[inStream]+ k byteRange <= RandFile`Private`TrueRandomDataMarkers[[curSeqPos,2]]+RandFile`Private`MaxTrueRandomSequenceLength,
		rawRndIntList=BinaryReadList[inStream, "UnsignedInteger32", k];			
		Do[
			Switch[rawRndInt,
				_Integer (* there are still data to read *),
						bits=IntegerDigits[rawRndInt,2];
						tmpReal=Reverse[bits].Map[2.0^-#&,Range[Length[bits]]];
						AppendTo[realList,tmpReal*b], (* scale to the [0,b] *)
				EndOfFile,	(* no more data to read - return Null*)
					Message[RandFileNoMoreDataError::argerr,fileName];
					Null
			],
		{rawRndInt,rawRndIntList}];
		realList,
				
	(* else for checking if the sequence is OK starts here *)
	Message[TrueRandomSequence::rangeoverlap];
	]
];


TrueRandomReal[{a_,b_},k_,inStream_InputStream]:=Block[{delta=Abs[b-a]},
	a + delta*TrueRandomReal[1,k,inStream]
];


TrueRandomReal[b_,dims_List,inStream_InputStream]:=TrueRandomReal[{0,b},dims,inStream];


TrueRandomReal[{a_,b_},dims_List,inStream_InputStream]:=ArrayReshape[TrueRandomReal[{a,b},Times@@dims,inStream],dims];


(* functions without inStream argument (assuming global input stream) *)


TrueRandomReal[]:=TrueRandomReal[RandFile`Private`TrueRandomDataStream];


TrueRandomReal[b_]:=TrueRandomReal[b,RandFile`Private`TrueRandomDataStream];


TrueRandomReal[{a_,b_}]:=TrueRandomReal[{a,b},RandFile`Private`TrueRandomDataStream];


TrueRandomReal[b_,n_Integer]:=TrueRandomReal[b,n,RandFile`Private`TrueRandomDataStream];


TrueRandomReal[{a_,b_},n_Integer]:=TrueRandomReal[{a,b},n,RandFile`Private`TrueRandomDataStream];


TrueRandomReal[b_,dims_List]:=TrueRandomReal[{0,b},dims,RandFile`Private`TrueRandomDataStream];


TrueRandomReal[{a_,b_},dims_List]:=TrueRandomReal[{a,b},dims,RandFile`Private`TrueRandomDataStream];


(* additional functions for the normal distribution *)


TrueRandomReal[dist_NormalDistribution,inStream_InputStream]:=Mean[dist]+Sqrt[2]Sqrt[Variance[dist]] InverseErf[-1+2 TrueRandomReal[inStream]];


TrueRandomReal[dist_NormalDistribution,n_Integer,inStream_InputStream]:=Mean[dist]+Sqrt[2]Sqrt[Variance[dist]] InverseErf[-1+2 TrueRandomReal[{0,1},n,inStream]];


TrueRandomReal[dist_NormalDistribution,dims_List,inStream_InputStream]:=Mean[dist]+Sqrt[2]Sqrt[Variance[dist]] InverseErf[-1+2 TrueRandomReal[{0,1},dims,inStream]];


TrueRandomReal[dist_NormalDistribution]:=TrueRandomReal[dist,RandFile`Private`TrueRandomDataStream];


TrueRandomReal[dist_NormalDistribution,n_Integer]:=TrueRandomReal[dist,n,RandFile`Private`TrueRandomDataStream];


TrueRandomReal[dist_NormalDistribution,dims_List]:=TrueRandomReal[dist,dims,RandFile`Private`TrueRandomDataStream];


(* ::Section::Closed:: *)
(*Random complex*)


TrueRandomComplex[{min_,max_},k_Integer,inStream_InputStream:RandFile`Private`TrueRandomDataStream]:=Block[{reRange=Abs[Re[max]-Re[min]],imRange=Abs[Im[max]-Im[min]]},
	TrueRandomReal[reRange,k,inStream] + I TrueRandomReal[imRange,k,inStream] + Min[Re[min],Re[max]] + I Min[Im[min],Im[max]]
];


TrueRandomComplex[inStream_InputStream:RandFile`Private`TrueRandomDataStream]:=TrueRandomComplex[{0,1+I},inStream];


TrueRandomComplex[max_,inStream_InputStream:RandFile`Private`TrueRandomDataStream]:=TrueRandomComplex[{0,max},inStream];


TrueRandomComplex[{min_,max_},inStream_InputStream:RandFile`Private`TrueRandomDataStream]:=TrueRandomComplex[{min,max},1,inStream];


TrueRandomComplex[{min_,max_},dims_List,inStream_InputStream:RandFile`Private`TrueRandomDataStream]:=ArrayReshape[TrueRandomComplex[{min,max},Times@@dims,inStream],dims];


(* ::Section::Closed:: *)
(*Deprecated Random[] interface*)


(* NOTE: This is added only to provide a basic compatibility layer and may fail to give the proper results in some cases. *)
TrueRandom[type_Symbol:Real,range_List:{0,1},k_Integer:1,inStream_InputStream:RandFile`Private`TrueRandomDataStream]:=Switch[type,
	Integer, TrueRandomInteger[range,k,inStream],
	Real, TrueRandomReal[range,k,inStream],
	Complex, TrueRandomComplex[range,k,inStream],
	_, Message[RandFileUnknownTypeError::argerr,type]
];


(* ::Section:: *)
(*TrueRandomChoice*)


TrueRandomChoice[list_List]:=TrueRandomChoice[Rule[ConstantArray[1,Length[list]],list]];


TrueRandomChoice[list_List,n_Integer]:=Table[TrueRandomChoice[list],{n}];


TrueRandomChoice[list_List,dims_List]:=ArrayReshape[TrueRandomChoice[list],dims];


TrueRandomChoice[rule_Rule,n_Integer]:=Table[TrueRandomChoice[rule],{n}];


TrueRandomChoice[rule_Rule,dims_List]:=ArrayReshape[TrueRandomChoice[rule],dims];


TrueRandomChoice[rule_Rule]:=Module[{sortRule,choiceWeights,indexReal,byteRange,curSeqPos},

	byteRange=4; (* we need only one 32-bit real number for each call *)
	
	If[Length[RandFile`Private`TrueRandomDataMarkers]>0,
		curSeqPos = Position[RandFile`Private`TrueRandomDataMarkers,RandFile`Private`TrueRandomDataCurrentMarker][[1,1]];
	];

	(*TODO: add check if we even use TrueRandomDataMarkers table *)
	(*SUGGESTION: Evaluation pattern similar to CallAsTable etc. but controlled with a global variable or a length of andFile`Private`TrueRandomDataMarkers array *)
	If[Length[RandFile`Private`TrueRandomDataMarkers]==0 || StreamPosition[RandFile`Private`TrueRandomDataFile]+byteRange <= RandFile`Private`TrueRandomDataMarkers[[curSeqPos,2]]+RandFile`Private`MaxTrueRandomSequenceLength,

		sortRule=Sort[{rule[[1]],rule[[2]]}\[Transpose],#1[[1]]<#2[[1]]&]\[Transpose];
		choiceWeights=TrueRandomChoiceIntervals[sortRule[[1]]];

		(* this functions makes use of the global source of the randomenss *)
		indexReal=TrueRandomReal[{0,Last[choiceWeights]}];

		Return[(sortRule[[2]])[[Position[Map[indexReal<=#&,choiceWeights],True][[1,1]]]]],

		(* else for checking if the sequence is OK starts here *)
		Message[TrueRandomSequence::rangeoverlap];
	];
];


TrueRandomChoiceIntervals[ruleWeights_]:=TrueRandomChoiceIntervals[ruleWeights]=Block[{choiceIntervals,extRuleWeights},
	extRuleWeights=Prepend[ruleWeights,0];
	choiceIntervals=ConstantArray[0,Length[extRuleWeights]];
	choiceIntervals[[1]]=extRuleWeights[[1]];
	choiceIntervals[[2]]=extRuleWeights[[2]];
	For[i=3,i<=Length[extRuleWeights],i++,
		choiceIntervals[[i]]=choiceIntervals[[i-1]]+(extRuleWeights[[i]]/extRuleWeights[[i-1]])(choiceIntervals[[i-1]]-choiceIntervals[[i-2]]);
	];
	choiceIntervals=Drop[choiceIntervals,1]
];


(* ::Section:: *)
(*TrueRandomSample*)


TrueRandomSample[list_List]:=TrueRandomChoice[Permutations[list]];


TrueRandomSample[list_List,n_Integer]:=Take[TrueRandomSample[list],n];


TrueRandomSample[rule_Rule,n_Integer]:=Block[{sample={}},
	While[Length[sample]<n,sample=Union[Append[sample,RandomChoice[Rule[w,list]]]]];
	sample
];


TrueRandomSample[rule_Rule,1]:=TrueRandomChoice[rule];


(* ::Section:: *)
(*Welcome message*)


Print["Package RandFile version ", RandFile`Private`randfileVersion, " (last modification: ", RandFile`Private`randfileLastModification, ")."];


Print["Usage notes:"];


Print["1) Almost all provided functions require to set a global variable pointing to file with random data! This can be done by using \!\(\*
StyleBox[\"SetTrueRandomDataFile\",\nFontWeight->\"Bold\"]\) function. For example \!\(\*
StyleBox[\"SetTrueRandomDataFile\",\nFontWeight->\"Bold\"]\)[\"/home/user_name/data/sample_file.bin\"] for GNU/Linux systems or \!\(\*StyleBox[\"SetTrueRandomDataFile\",\nFontWeight->\"Bold\"]\)[\"/Users/user_name/data/sample_file.bin\"] for OS X systems. Please mind that it is advised to use this function only once during the session."];


Print["2) If you intend to use \!\(\*
StyleBox[\"TrueRandomSequence\",\nFontWeight->\"Bold\"]\) function you must use \!\(\*
StyleBox[\"SetMaxTrueRandomSequenceLength\",\nFontWeight->\"Bold\"]\) and declare at least one sequence. Currently declared sequences can be displayed by calling \!\(\*
StyleBox[\"GetTrueRandomDataMarkers\",\nFontWeight->\"Bold\"]\)[]. Once defined, the used maximal length cannot be changed during the session."];


End[];


(* ::Section::Closed:: *)
(*Footer*)


Protect@@Names["RandFile`*"];


EndPackage[];
