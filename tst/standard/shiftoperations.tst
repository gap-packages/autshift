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
gap> T := IdentityTransducer(3);;
gap> IsLipschitzTransducer(T);
true
gap> T := Transducer(3, 3, [[2, 3, 3], [2, 4, 2], [3, 3, 3], [3, 1, 4]],
> [[[], [1, 1, 2, 1], [1, 1]], [[0], [], [1]], [[1], [0], [2]],
> [[2], [0, 1, 2], [2]]]);;
gap> IsLipschitzTransducer(T);
true
gap> T := Transducer(3, 3, [[2, 1, 3], [4, 4, 3], [5, 2, 2], [3, 4, 2],
> [2, 3, 4]], [[[1, 0], [1], [0, 2]], [[2], [1], [1]], [[], [1], [0]],
> [[1], [1], [0]], [[0, 0], [1, 2], [0,1]]]);;
gap> IsLipschitzTransducer(T);
true
gap> T := Transducer(2, 2, [[2, 1], [1, 1]], [[[0], [1]], [[1], [0]]]);;
gap> IsLipschitzTransducer(T);
true
gap> T := Transducer(2, 2, [[2, 2], [1, 1]],[[[],[]],[[1],[1]]]);;
gap> IsLipschitzTransducer(T);
true
gap> T := AlphabetChangeTransducer(4, 3);;
gap> IsLipschitzTransducer(T);
true
gap> T := Transducer(3, 3, [[2, 2, 1], [3, 4, 3], [1, 1, 2], [2, 2, 3]],
> [[[1, 0, 1], [0], [2, 2, 0]], [[], [2], [0, 1, 2, 2]], [[], [0, 1], [2, 2]],
> [[2, 2], [0, 2], [1, 2, 2]]]);;
gap> IsLipschitzTransducer(T);
false
gap> T := AlphabetChangeTransducer(3, 4);;
gap> IsLipschitzTransducer(T);
false
gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsLipschitzTransducer(T);
false

#T# TransducerCore
gap> T := Transducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
gap> C := TransducerCore(T);;
gap> OutputFunction(C);
[ [ [ 1 ], [ 0, 1 ] ], [ [ 1 ], [  ] ], [ [ 1 ], [ 0 ] ] ]
gap> TransitionFunction(C);
[ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> C := TransducerCore(T);;
gap> OutputFunction(C);
[ [ [ 1, 0 ], [  ] ], [ [ 0 ], [ 1, 1 ] ], [ [ 0 ], [ 1 ] ] ]
gap> TransitionFunction(C);
[ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]

#T# IsCoreTransducer(T);
gap> T := Transducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
gap> IsCoreTransducer(T);
false
gap> C := TransducerCore(T);;
gap> IsCoreTransducer(C);
true
gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsCoreTransducer(T);
true
gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
> [1, 1]], [[0], [1]]]);;
gap> IsCoreTransducer(T);
true
gap> T := Transducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
> [[2], [3]]]);;
gap> IsCoreTransducer(T);
false
gap> T := DeBruijnTransducer(2, 3);;
gap> IsCoreTransducer(T);
true
gap> T := DeBruijnTransducer(3, 2);;
gap> IsCoreTransducer(T);
true

