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

# IdentityShiftIsomorphism
gap> IdentityShiftIsomorphism(3);
<shift isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 1 state.>
gap> IdentityShiftIsomorphism(2);
<shift isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 1 state.>

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

# MinimalUDAFTransducer
gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> T := UDAFTransducer(L01, L10);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 2 states.>
gap> M := MinimalUDAFTransducer(T);
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
2 edges, and which has 4 states.>
gap> T2 := ComposeUDAFTransducers(T, T);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 4 states.>
gap> f := T2!.DomainFolding;
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> g := T2!.CoDomainFolding;
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> MaxHistoryConeDepth(f);
1
gap> MaxFutureConeDepth(f); 
1
gap> MaxHistoryConeDepth(g);
2
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

# ResizeZeroStringTransducer (and BlockCodeTransducer as this is used in
# zero string)
gap> T := ResizeZeroStringTransducer(3, 1, 2);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 4 states.>
gap> TransitionFunction(T);
[ [ 1, 2, 2 ], [ 3, 2, 2 ], [ 4, 2, 2 ], [ 1, 2, 2 ] ]
gap> OutputFunction(T);
[ [ [ 0 ], [ 1 ], [ 2 ] ], [ [ 0 ], [ 1 ], [ 2 ] ], [ [  ], [ 0, 1 ], [ 2 ] ],
  [ [ 0, 0 ], [ 1 ], [ 0, 2 ] ] ]
gap> T := ResizeZeroStringTransducer(2, 2, 3);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>
gap> TransitionFunction(T);
[ [ 1, 2 ], [ 3, 2 ], [ 4, 2 ], [ 5, 2 ], [ 1, 2 ] ]
gap> OutputFunction(T);
[ [ [ 0 ], [ 1 ] ], [ [ 0 ], [ 1 ] ], [ [ 0 ], [ 1 ] ], [ [  ], [ 0, 1 ] ], 
  [ [ 0, 0 ], [ 1 ] ] ]
gap> ResizeZeroStringTransducer(1, 2, 3);
Error, autshift: BlockCodeTransducer: usage,
the alphabet must have at least two letters,

#ShiftIsomorphism
gap> T := IdentityShiftIsomorphism(3);
<shift isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 1 state.>
gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1, 1, 1]])));
<shift isomorphism whose domain digraph has 
4 edges, whose codomain digraph has 4 edges, and which has 1 state.>
gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])),
> [-1]);
<shift isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])),
> [1]); 
<shift isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])),
> [1]); 
<shift isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
gap> S!.Annotation;
[ 1 ]
gap> (S^3)!.Annotation;
[ 3 ]
gap> (S^-2)!.Annotation;
[ -2 ]
gap> Sm4 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])),
> [-4]);
<shift isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 16 states.>
gap> Sm4 = S^-4;
true
gap> Sm4 = S^4; 
false

#UDAFIsomorphism
gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> T := UDAFIsomorphism(L01, L10);
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
gap> M := T^0;
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
gap> T = M;
true
gap> R := ResizeZeroStringTransducer(2, 2, 3);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>
gap> U := UDAFIsomorphism(R);
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
gap> U^2;
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
gap> R := ResizeZeroStringTransducer(3, 1, 3);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 5 states.>
gap> U := UDAFIsomorphism(R);                 
<UDAF Isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 5 states.>

#Speed
gap> T := ResizeZeroStringTransducer(3, 1, 2);;
gap> U := UDAFTransducer(T);
<UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
3 edges, and which has 4 states.>
gap> T := ResizeZeroStringTransducer(3, 1, 2);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 4 states.>
gap> U := UDAFTransducer(T);
<UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
3 edges, and which has 4 states.>
gap> I := U^-1;
<UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
3 edges, and which has 4 states.>
gap> AreIsomorphicUDAFTransducers(I, U);
false
gap> M := MinimalUDAFTransducer(I);
<UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
3 edges, and which has 4 states.>
gap> AreIsomorphicUDAFTransducers(M, U);
true
gap> R := ResizeZeroStringTransducer(2, 2, 3);          
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>
gap> R := ResizeZeroStringTransducer(2, 2, 3);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>
gap> R2 := ResizeZeroStringTransducer(2, 1, 3);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 5 states.>
gap> R3 := ResizeZeroStringTransducer(2, 1, 2);
<transducer with input alphabet on 2 symbols, output alphabet on 
2 symbols, and 4 states.>
gap> U := UDAFTransducer(R);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 5 states.>
gap> U2 := UDAFTransducer(R2);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 5 states.>
gap> U3 := UDAFTransducer(R3);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 4 states.>
gap> MinimalUDAFTransducer(U2^-1);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 5 states.>
gap> AreIsomorphicUDAFTransducers(MinimalUDAFTransducer(U2^-1), U2);
true
gap> M := MinimalUDAFTransducer(U * U2);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 5 states.>
gap> P := M * U;
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 16 states.>
gap> M := MinimalUDAFTransducer(P);
<UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
2 edges, and which has 4 states.>
gap> U := UDAFIsomorphism(U);
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
gap> U2 := UDAFIsomorphism(U2);
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
gap> U3 := UDAFIsomorphism(U3);
<UDAF Isomorphism whose domain digraph has 
2 edges, whose codomain digraph has 2 edges, and which has 4 states.>
gap> U*U2*U3 = U2; 
true

# IsOneSidedFolding
gap> H := WalkHomomorphism(Digraph([[1, 1]]),Digraph([[1,1]]),[1],[[2],[1]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> I := IdentityWalkHomomorphism(Digraph([[1, 1]]));
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> IsOneSidedFolding(H);         
true
gap> IsOneSidedFolding(I);                                                   
true
gap> H := WalkHomomorphism(Digraph([[1, 1]]),Digraph([[1,1]]),[1],[[2],[2]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> IsOneSidedFolding(H);
false
gap> D := Digraph([[1, 2, 2], []]);
<immutable multidigraph with 2 vertices, 3 edges>
gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [3], [2]]);
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> IsOneSidedFolding(W);
true
gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [2], [2]]);
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> IsOneSidedFolding(W);                                
false
gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [1, 3], [2]]);
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> IsOneSidedFolding(W);                                   
false
gap> W := WalkHomomorphism(D, D, [1, 2], [[], [1, 3], [2]]); 
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> IsOneSidedFolding(W);                                  
false

# OneSidedTorsionDecomposition
gap> C := Transducer(3, 3, [[1, 1, 1]], [[[1], [2], [0]]]);
<transducer with input alphabet on 3 symbols, output alphabet on 
3 symbols, and 1 state.>
gap> C:= OneSidedShiftIsomorphism(UDAFTransducer(C));
<one sided shift isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 1 state.>
gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
> Digraph([[1, 1, 1]]),
> [1, 1, 1],
> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
<walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
> Digraph([[1, 1, 1]]),
> [1, 1, 1],
> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
<walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
gap> Fig5 := OneSidedShiftIsomorphism(Fig5L, Fig5R);  
<one sided shift isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
gap> Fig5^C;
<one sided shift isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
> Digraph([[1, 1, 1]]),
> [1, 1, 1],
> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
<walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
> Digraph([[1, 1, 1]]),
> [1, 1, 1],
> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
<walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
gap>  Fig5 := OneSidedShiftIsomorphism(Fig5L, Fig5R);      
<one sided shift isomorphism whose domain digraph has 
3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
gap>  L := OneSidedTorsionDecomposition(Fig5);         
[ <one sided shift isomorphism whose domain digraph has 
    3 edges, whose codomain digraph has 3 edges, and which has 1 state.>, 
  <one sided shift isomorphism whose domain digraph has 
    3 edges, whose codomain digraph has 3 edges, and which has 2 states.> ]
gap> L[1] * L[2] = Fig5;
true
gap> f := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
> [ [ 3 ], [ 4 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 1 ],
> [ 2 ] ]);
<walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
gap> g := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
> [ [ 3 ], [ 4 ], [ 4 ], [ 3 ], [ 2 ], [ 1 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], 
> [ 1 ], [ 2 ] ]);
<walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
gap> T := OneSidedShiftIsomorphism(f, g);
<one sided shift isomorphism whose domain digraph has 
4 edges, whose codomain digraph has 4 edges, and which has 6 states.>
gap> S := OneSidedTorsionDecomposition(T);
[ <one sided shift isomorphism whose domain digraph has 
    4 edges, whose codomain digraph has 4 edges, and which has 3 states.>, 
  <one sided shift isomorphism whose domain digraph has 
    4 edges, whose codomain digraph has 4 edges, and which has 5 states.> ]
gap> S[1] * S[2] = T;
true

# RandomOneSidedAut
gap> T := RandomOneSidedAut(Digraph([[1, 1, 1]]));;
gap> T := RandomOneSidedAut(Digraph([[1, 1]]));;   
gap> T := RandomOneSidedAut(Digraph([[1, 2], [1, 2]]));;
gap> T := RandomOneSidedAut(Digraph([[1, 2], [1]]));;   
gap> T := RandomOneSidedAut(Digraph([[1, 1, 2], [1]]));;
gap> T := RandomOneSidedAut(Digraph([[2, 2], [1, 1]]));;
gap> D := (Digraph([[1,1, 2, 2], []]));;
gap> RandomOneSidedAut(D);;
gap> RandomOneSidedAut(D);;
gap> RandomOneSidedAut(D);;
gap> RandomOneSidedAut(D);;
gap> RandomOneSidedAut(D);;
gap> RandomOneSidedAut(D);;
gap> RandomOneSidedAut(D);;