############################################################################
##
#W  standard/shiftoperations.tst
#Y  Copyright (C) 2022                                 Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("autshift package: standard/shiftoperations.tst");
gap> LoadPackage("autshift", false);;

#
gap> STOP_TEST("autshift package: standard/shiftoperations.tst");


#T# IsLipschitzTransducer
gap> T := IdentityGNSTransducer(3);;
gap> IsLipschitzGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[2, 3, 3], [2, 4, 2], [3, 3, 3], [3, 1, 4]],
> [[[], [1, 1, 2, 1], [1, 1]], [[0], [], [1]], [[1], [0], [2]],
> [[2], [0, 1, 2], [2]]]);;
gap> IsLipschitzGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[2, 1, 3], [4, 4, 3], [5, 2, 2], [3, 4, 2],
> [2, 3, 4]], [[[1, 0], [1], [0, 2]], [[2], [1], [1]], [[], [1], [0]],
> [[1], [1], [0]], [[0, 0], [1, 2], [0,1]]]);;
gap> IsLipschitzGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[2, 1], [1, 1]], [[[0], [1]], [[1], [0]]]);;
gap> IsLipschitzGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[2, 2], [1, 1]],[[[],[]],[[1],[1]]]);;
gap> IsLipschitzGNSTransducer(T);
true
gap> T := AlphabetChangeGNSTransducer(4, 3);;
gap> IsLipschitzGNSTransducer(T);
true
gap> T := GNSTransducer(3, 3, [[2, 2, 1], [3, 4, 3], [1, 1, 2], [2, 2, 3]],
> [[[1, 0, 1], [0], [2, 2, 0]], [[], [2], [0, 1, 2, 2]], [[], [0, 1], [2, 2]],
> [[2, 2], [0, 2], [1, 2, 2]]]);;
gap> IsLipschitzGNSTransducer(T);
false
gap> T := AlphabetChangeGNSTransducer(3, 4);;
gap> IsLipschitzGNSTransducer(T);
false
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsLipschitzGNSTransducer(T);
false

#T# GNSTransducerCore
gap> T := GNSTransducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
gap> C := GNSTransducerCore(T);;
gap> OutputFunction(C);
[ [ [ 1 ], [ 0, 1 ] ], [ [ 1 ], [  ] ], [ [ 1 ], [ 0 ] ] ]
gap> TransitionFunction(C);
[ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> C := GNSTransducerCore(T);;
gap> OutputFunction(C);
[ [ [ 1, 0 ], [  ] ], [ [ 0 ], [ 1, 1 ] ], [ [ 0 ], [ 1 ] ] ]
gap> TransitionFunction(C);
[ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
gap> T := GNSTransducer(2, 2, [[2, 2], [1, 1]], [[[1], [1]], [[0], [0]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>
gap> GNSTransducerCore(T);
Error, autshift: GNSTransducerCore: usage,
the transducer must be synchronizing 

#T# IsCoreGNSTransducer(T);
gap> T := GNSTransducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
gap> IsCoreGNSTransducer(T);
false
gap> C := GNSTransducerCore(T);;
gap> IsCoreGNSTransducer(C);
true
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsCoreGNSTransducer(T);
true
gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsCoreGNSTransducer(T);
true
gap> T := GNSTransducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
> [[2], [3]]]);;
gap> IsCoreGNSTransducer(T);
false
gap> T := DeBruijnGNSTransducer(2, 3);;
gap> IsCoreGNSTransducer(T);
true
gap> T := DeBruijnGNSTransducer(3, 2);;
gap> IsCoreGNSTransducer(T);
true
