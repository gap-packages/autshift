#############################################################################
##
#W  shiftoperations.gd
#Y  Copyright (C) 2022                               Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declaration of operations that relate to transducers.


#! @Arguments T
#! @Returns true or false
#! @Description
#!  We say that a trasducer T is Lipschitz if for each state of T defines a 
#!  well-defined function from the set of forwards infinite words in the domain 
#!  alphabet to the set of infinite words, and these maps are all Lipschitz 
#!  continuous.
#!  TODO add disclaimer and metric
#! <Example>
#!gap> T := Transducer(2, 2, [[2, 2], [1, 1]],[[[],[]],[[1],[1]]]);;
#!gap> IsLipschitzTransducer(T);
#!true
#!gap> T := AlphabetChangeTransducer(4, 3);;
#!gap> IsLipschitzTransducer(T);
#!true
#!gap> T := Transducer(3, 3, [[2, 2, 1], [3, 4, 3], [1, 1, 2], [2, 2, 3]],
#!> [[[1, 0, 1], [0], [2, 2, 0]], [[], [2], [0, 1, 2, 2]], [[], [0, 1], [2, 2]],
#!> [[2, 2], [0, 2], [1, 2, 2]]]);;
#!gap> IsLipschitzTransducer(T);
#!false
#!gap> T := AlphabetChangeTransducer(3, 4);;
#!gap> IsLipschitzTransducer(T);
#!false
#!gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> IsLipschitzTransducer(T);
#!false
#!gap> M := MinimalTransducer(T);                                      
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 2 states.>
#!gap> IsLipschitzTransducer(T);                                          
#!true
#!gap> IsLipschitzTransducer(M);                                          
#!false
#!</Example>
DeclareOperation("IsLipschitzTransducer", [IsTransducer]);

#! @Arguments T
#! @Returns an aaa transducer
#! @Description
#!  If T is a synchronizing transducer, then we define the core of T to be the
#!  smallest non-empty transducer obtainable from T by removing states from T.
#!
#!  The Operation takes as input an synchronizing transducer and returns its Core
#! <Example>
#!gap> T := Transducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
#!> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
#!gap> C := TransducerCore(T);;
#!gap> OutputFunction(C);
#![ [ [ 1 ], [ 0, 1 ] ], [ [ 1 ], [  ] ], [ [ 1 ], [ 0 ] ] ]
#!gap> TransitionFunction(C);
#![ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
#!gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> C := TransducerCore(T);;
#!gap> OutputFunction(C);
#![ [ [ 1, 0 ], [  ] ], [ [ 0 ], [ 1, 1 ] ], [ [ 0 ], [ 1 ] ] ]
#!gap> TransitionFunction(C);
#![ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
#!gap> T := Transducer(2, 2, [[2, 2], [1, 1]], [[[1], [1]], [[0], [0]]]);
#!transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 2 states.>
#!gap> TransducerCore(T);
#!Error, autshift: TransducerCore: usage,
#!the transducer must be synchronizing 
#!</Example>
DeclareOperation("TransducerCore", [IsTransducer]);

#! @Arguments T
#! @Returns true or false
#! @Description
#!  If T is a synchronizing transducer, then we define the core of T to be the
#!  smallest non-empty transducer obtainable from T by removing states from T.
#!  We say that T is core if it is equal to its core.
#!
#!  The attribute returns true if and only if the given transducer is core.
#! <Example>
#!gap> T := Transducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
#!> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
#!gap> IsCoreTransducer(T);
#!false
#!gap> C := TransducerCore(T);;
#!gap> IsCoreTransducer(C);
#!true
#!gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> IsCoreTransducer(T);
#!true
#!gap> T := Transducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> IsCoreTransducer(T);
#!true
#!gap> T := Transducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
#!> [[2], [3]]]);;
#!gap> IsCoreTransducer(T);
#!false
#!gap> T := DeBruijnTransducer(2, 3);;
#!gap> IsCoreTransducer(T);
#!true
#!gap> T := DeBruijnTransducer(3, 2);;
#!gap> IsCoreTransducer(T);
#!true
#!</Example>
DeclareAttribute("IsCoreTransducer", IsTransducer);
#DeclareOperation("ImageAsUnionOfCones", [IsTransducer]);
#DeclareOperation("HasClopenImage", [IsTransducer]);

