############################################################################
##
#W  standard/transducer.tst
#Y  Copyright (C) 2022                                 Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("autshift package: standard/transducer.tst");
gap> LoadPackage("autshift", false);;

#
gap> STOP_TEST("autshift package: standard/transducer.tst");


#T# DeBruijinTransducer
gap> T := DeBruijnTransducer(2, 2);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 4 states.>
gap> TransitionFunction(T);
[ [ 1, 3 ], [ 1, 3 ], [ 2, 4 ], [ 2, 4 ] ]
gap> OutputFunction(T);
[ [ [ 0 ], [ 1 ] ], [ [ 0 ], [ 1 ] ], [ [ 0 ], [ 1 ] ], [ [ 0 ], [ 1 ] ] ]
gap> T := DeBruijnTransducer(3, 4);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 81 states.>
gap> TransitionFunction(T);
[ [ 1, 28, 55 ], [ 1, 28, 55 ], [ 1, 28, 55 ], [ 2, 29, 56 ], [ 2, 29, 56 ], 
  [ 2, 29, 56 ], [ 3, 30, 57 ], [ 3, 30, 57 ], [ 3, 30, 57 ], [ 4, 31, 58 ], 
  [ 4, 31, 58 ], [ 4, 31, 58 ], [ 5, 32, 59 ], [ 5, 32, 59 ], [ 5, 32, 59 ], 
  [ 6, 33, 60 ], [ 6, 33, 60 ], [ 6, 33, 60 ], [ 7, 34, 61 ], [ 7, 34, 61 ], 
  [ 7, 34, 61 ], [ 8, 35, 62 ], [ 8, 35, 62 ], [ 8, 35, 62 ], [ 9, 36, 63 ], 
  [ 9, 36, 63 ], [ 9, 36, 63 ], [ 10, 37, 64 ], [ 10, 37, 64 ], 
  [ 10, 37, 64 ], [ 11, 38, 65 ], [ 11, 38, 65 ], [ 11, 38, 65 ], 
  [ 12, 39, 66 ], [ 12, 39, 66 ], [ 12, 39, 66 ], [ 13, 40, 67 ], 
  [ 13, 40, 67 ], [ 13, 40, 67 ], [ 14, 41, 68 ], [ 14, 41, 68 ], 
  [ 14, 41, 68 ], [ 15, 42, 69 ], [ 15, 42, 69 ], [ 15, 42, 69 ], 
  [ 16, 43, 70 ], [ 16, 43, 70 ], [ 16, 43, 70 ], [ 17, 44, 71 ], 
  [ 17, 44, 71 ], [ 17, 44, 71 ], [ 18, 45, 72 ], [ 18, 45, 72 ], 
  [ 18, 45, 72 ], [ 19, 46, 73 ], [ 19, 46, 73 ], [ 19, 46, 73 ], 
  [ 20, 47, 74 ], [ 20, 47, 74 ], [ 20, 47, 74 ], [ 21, 48, 75 ], 
  [ 21, 48, 75 ], [ 21, 48, 75 ], [ 22, 49, 76 ], [ 22, 49, 76 ], 
  [ 22, 49, 76 ], [ 23, 50, 77 ], [ 23, 50, 77 ], [ 23, 50, 77 ], 
  [ 24, 51, 78 ], [ 24, 51, 78 ], [ 24, 51, 78 ], [ 25, 52, 79 ], 
  [ 25, 52, 79 ], [ 25, 52, 79 ], [ 26, 53, 80 ], [ 26, 53, 80 ], 
  [ 26, 53, 80 ], [ 27, 54, 81 ], [ 27, 54, 81 ], [ 27, 54, 81 ] ]
gap> OutputFunction(T);
[ [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], 
  [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ] ]

# IdentityShiftMorphism
gap> IdentityShiftMorphism(3);
<morphism with input alphabet on 3 symbols, output alphabet on 3 symbols, and 
1 state.>
gap> IdentityShiftMorphism(2);
<morphism with input alphabet on 2 symbols, output alphabet on 2 symbols, and 
1 state.>

# UDAFTransducer with GNS transducer input
gap> T := Transducer(2, 2, [[1, 1]], [[[], []]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 1 state.>
gap> UDAFTransducer(T);
Error, autshift: UDAFTransducer: usage,
the transducer must not be degenerate,
gap> T := Transducer(2, 2, [[2, 2], [1, 1]], [[[1], [1]], [[0], [0]]]);        
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>
gap> UDAFTransducer(T);                                                
Error, autshift: UDAFTransducer: usage,
the transducer must be sychronizing,
gap> T := Transducer(2, 2, [[2, 2], [2, 2]], [[[1], [1]], [[0], [0]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 2 states.>
gap> UDAFTransducer(T);
Error, autshift: UDAFTransducer: usage,
the transducer must be core,
gap> T := Transducer(2, 2, [[1, 1]], [[[1], [1]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 1 state.>
gap> UDAFTransducer(T);
Error, autshift: UDAFTransducer: usage,
the transducer must be UDAF invertible,
gap> T := Transducer(2, 2, [[1, 1]], [[[1], [0]]]);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 1 state.>
gap> UDAFTransducer(T);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 1 state.>

# UDAFTransducer With UDAF folding input
gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> T := UDAFTransducer(L01, L10);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 2 states.>
gap> D := L01!.DomainDigraph;
<immutable digraph with 2 vertices, 4 edges>
gap> W := WalkHomomorphism(D, D, [1, 1], [[], [], [], []]);
<walk homomorphism from a digraph with 4 edges to a digraph with 4 edges.>
gap> T := UDAFTransducer(L01, W);
Error, autshift: UDAFTransducer: usage,
the walk homomorphisms must be UDAF foldings,
gap> T := UDAFTransducer(L01, PhitoR2Fold());
Error, autshift: UDAFTransducer: usage,
the walk homomorphisms must have the same domain,

# MinimiseUDAFTransducer
gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> T := UDAFTransducer(L01, L10);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 2 states.>
gap> M := MinimiseUDAFTransducer(T);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 1 state.>
gap> M = IdentityUDAFTransducer(Digraph([[1, 1]]));
true

# ComposeUDAFTransducers
gap> L11 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 1);               
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> L20 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 2, 0);
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> T := UDAFTransducer(L11, L20);                              
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 4 states.>
gap> ComposeUDAFTransducers(T, T);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 8 states.>
gap> T2 := ComposeUDAFTransducers(T, T);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 8 states.>
gap> f := T2!.DomainFolding;
<walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
gap> g := T2!.CoDomainFolding;
<walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
gap> MaxHistoryConeDepth(f);
1
gap> MaxFutureConeDepth(f); 
2
gap> MaxHistoryConeDepth(g);
3
gap> MaxFutureConeDepth(g); 
0

# ^-1 * and =
gap> L20 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 2, 0);
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> L11 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 1);
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> T := UDAFTransducer(L20, L11);           
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 4 states.>
gap> T * T = ComposeUDAFTransducers(T, T);
true
gap> T^-1 = UDAFTransducer(L11, L20);
true
