############################################################################
##
#W  standard/foldings.tst
#Y  Copyright (C) 2022                                 Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##
gap> START_TEST("autshift package: standard/foldings.tst");
gap> LoadPackage("autshift", false);;

#
gap> STOP_TEST("autshift package: standard/foldings.tst");


# WalkHomomorphism
gap> WalkHomomorphism(Digraph([[1, 2], [1]]), Digraph([[1, 1]]), [1, 1],
> [[1], [2], []]);
<walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.>
gap> WalkHomomorphism(Digraph([[1, 2], [1]]), Digraph([[1, 1]]), [1],
> [[1], [2], []]);
Error, AutShift: WalkHomomorphism: usage,
the third input must have one entry for each vertex of
 the first input,
gap> WalkHomomorphism(Digraph([[1, 2], [1]]), Digraph([[1, 1]]), [1, 1],
> [[2], [1]]);
Error, AutShift: WalkHomomorphism: usage,
the fourth input must have one entry for each edge of
 the first input,
gap> WalkHomomorphism(Digraph([[1, 2], [1]]), Digraph([[1, 1]]), [1, 0],
> [[1], [1], []]);
Error, AutShift: WalkHomomorphism: usage,
the third input must be a list of vertices of the second
input,
gap> WalkHomomorphism(Digraph([[1, 2], [1]]), Digraph([[1, 1]]), [1, 1],
> [[1], [0], []]);
Error, AutShift: WalkHomomorphism: usage,
the fourth input must be a list of edge walks in the second
input,
gap> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1],
> [[1], [2, 3]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 3 edges.>
gap> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1],
> [[1], [3, 2]]);
Error, AutShift: WalkHomomorphism: usage,
the walks in the forth input must be compatible with
 the vertices in the third input,
gap> H := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1],
> [[1], [2, 3]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 3 edges.>
gap> H!.DomainDigraph;
<immutable multidigraph with 1 vertex, 2 edges>
gap> H!.CoDomainDigraph;
<immutable digraph with 2 vertices, 3 edges>
gap> H!.VertexMap;
[ 1 ]
gap> H!.EdgeMap;
[ [ 1 ], [ 2, 3 ] ]

# PhitoR2Fold and R2toPhiFold
gap> H := PhitoR2Fold();
<walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.>
gap> H!.DomainDigraph;
<immutable digraph with 2 vertices, 3 edges>
gap> H!.CoDomainDigraph;
<immutable multidigraph with 1 vertex, 2 edges>
gap> H!.VertexMap;
[ 1, 1 ]
gap> H!.EdgeMap;
[ [ 1 ], [ 2 ], [  ] ]
gap> H2 := R2toPhiFold();
<walk homomorphism from a digraph with 2 edges to a digraph with 3 edges.>
gap> H2!.DomainDigraph;
<immutable multidigraph with 1 vertex, 2 edges>
gap> H2!.CoDomainDigraph;
<immutable digraph with 2 vertices, 3 edges>
gap> H2!.VertexMap;
[ 1 ]
gap> H2!.EdgeMap;
[ [ 1 ], [ 2, 3 ] ]

# ComposeWalkHomomorphisms
gap> H := ComposeWalkHomomorphisms(PhitoR2Fold(), R2toPhiFold());
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> H!.DomainDigraph;
<immutable digraph with 2 vertices, 3 edges>
gap> H!.CoDomainDigraph;
<immutable digraph with 2 vertices, 3 edges>
gap> H!.VertexMap;
[ 1, 1 ]
gap> H!.EdgeMap;
[ [ 1 ], [ 2, 3 ], [  ] ]
gap> H2 := R2toPhiFold() * PhitoR2Fold();
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> H2!.DomainDigraph;
<immutable multidigraph with 1 vertex, 2 edges>
gap> H2!.CoDomainDigraph;
<immutable multidigraph with 1 vertex, 2 edges>
gap> H2!.VertexMap;
[ 1 ]
gap> H2!.EdgeMap;
[ [ 1 ], [ 2 ] ]

# IsDegenerateWalkHomomorphism
gap> IsDegenerateWalkHomomorphism(PhitoR2Fold());
false
gap> IsDegenerateWalkHomomorphism(
> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1]]), [1], [[1], []]));
true
gap> IsDegenerateWalkHomomorphism(
> WalkHomomorphism(Digraph([[2], [1]]), Digraph([[1]]), [1, 1], [[], []]));
true

# IsUDAFDigraph
gap> IsUDAFDigraph(Digraph([[1, 1]]));
true
gap> IsUDAFDigraph(Digraph([[1]]));
false
gap> IsUDAFDigraph(Digraph([[], []]));
true
gap> IsUDAFDigraph(Digraph([[2], [1]]));
false
gap> IsUDAFDigraph(Digraph([[1, 2], [1]]));
true
gap> IsUDAFDigraph(Digraph([[1, 1], [1]]));
true
gap> IsUDAFDigraph(Digraph([[2, 2], [2]]));
false
gap> IsUDAFDigraph(Digraph([[2], [2, 2]]));
true
gap> D := IsUDAFDigraph(Digraph([[1, 1, 2], [3], []]));
true
gap> D := IsUDAFDigraph(Digraph([[1, 1, 2], [3], [2]]));
false

# MakeSynchronousWalkHomomorphism
gap> MakeSynchronousWalkHomomorphism(
> WalkHomomorphism(Digraph([[]]), Digraph([[]]), [1], []));
[ <walk homomorphism from a digraph with 0 edges to a digraph with 0 edges.>, 
  <walk homomorphism from a digraph with 0 edges to a digraph with 0 edges.> ]
gap> MakeSynchronousWalkHomomorphism(
> WalkHomomorphism(Digraph([[1]]), Digraph([[]]), [1], [[]]));
Error, AutShift: MakeSynchronousWalkHomomorphism: usage,
the given homomorphism must be non-degenerate
gap> MakeSynchronousWalkHomomorphism(
> WalkHomomorphism(Digraph([[]]), Digraph([[1]]), [1], []));
Error, AutShift: MakeSynchronousWalkHomomorphism: usage,
the target digraph must be an UDAF Digraph
gap> P := MakeSynchronousWalkHomomorphism(PhitoR2Fold());
[ <walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
  <walk homomorphism from a digraph with 4 edges to a digraph with 3 edges.> ]
gap> IsSynchronousWalkHomomorphism(P[1]);
true
gap> H := P[2] * PhitoR2Fold();
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> RemoveIncompleteResponse(H)[1] = RemoveIncompleteResponse(P[1])[1];
true
gap> P := MakeSynchronousWalkHomomorphism(R2toPhiFold());
[ <walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
  <walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.> ]
gap> H := P[2] * R2toPhiFold();
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> RemoveIncompleteResponse(H)[1] = RemoveIncompleteResponse(P[1])[1];
true

# WalkHomomorphismImageAutomaton
gap> WalkHomomorphismVertexImageAutomaton(R2toPhiFold(), 1);
< epsilon automaton on 4 letters with 2 states >
gap> WalkHomomorphismVertexImageAutomaton(PhitoR2Fold(), 1);
< epsilon automaton on 3 letters with 2 states >
gap> WalkHomomorphismVertexImageAutomaton(PhitoR2Fold(), 2);
< epsilon automaton on 3 letters with 2 states >
gap> WalkHomomorphismVertexImageAutomaton(R2toPhiFold(), 2);
Error, AutShift: WalkHomomorphismVertexImageAutomaton: usage,
the given integer must be a vertex of the doman digraph

# PowerSetWalkHomomorphism
gap> W := WalkHomomorphism(Digraph([[2, 2], [2, 2]]), Digraph([[1, 1]]), [1, 1], [[2, 1], [2, 1], [1], [2]]);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> ImageAsUnionOfCones(W, 1);                                                                              
[ [ [ 2, 1 ], 1 ] ]
gap> W2 := PowerSetWalkHomomorphism(W);                                                                      
<walk homomorphism from a digraph with 6 edges to a digraph with 2 edges.>
gap> ImageAsUnionOfCones(W2, 1);       
[ [ [ 2, 1 ], 1 ] ]

# IsUDAFFolding
gap> IsUDAFFolding(R2toPhiFold());
true
gap> IsUDAFFolding(PhitoR2Fold());
true
gap> IsUDAFFolding(IdentityWalkHomomorphism(Digraph([[], [1,3], []])));
true
gap> IsUDAFFolding(IdentityWalkHomomorphism(Digraph([[2], [1,3], []])));
false
gap> IsUDAFFolding(IdentityWalkHomomorphism(Digraph([[2], [1,2, 3], []])));
true
gap> IsUDAFFolding(
> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]), [1], [[1], []]));
false
gap> IsUDAFFolding(
> WalkHomomorphism(Digraph([[1, 1], [2, 2]]), Digraph([[1, 1]]),
> [1, 1], [[1], [2], [1], [2]]));
false
gap> IsUDAFFolding(
> WalkHomomorphism(Digraph([[1, 1, 1]]), Digraph([[1, 1]]),
> [1], [[1], [2], [1]]));
false
gap> f:= WalkHomomorphism(Digraph([ [ 1, 2, 3 ], [ 1, 4, 1 ], [ 1, 2, 3 ], 
> [ 1, 4, 1 ] ]), Digraph([ [ 3 ], [ 3 ], [ 3, 3, 3 ] ]), [ 3, 3, 3, 3 ], 
> [ [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], 
> [ 4 ], [ 5 ] ]);
<walk homomorphism from a digraph with 12 edges to a digraph with 5 edges.>
gap> IsUDAFFolding(f);
true

#FoldingToLineFolding
gap> FoldingToLineFolding(R2toPhiFold());
[ <walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
  <walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.> ]
gap> P := FoldingToLineFolding(R2toPhiFold());
[ <walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
  <walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.> ]
gap> H := P[2] * R2toPhiFold();
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> RemoveIncompleteResponse(P[1])[1] = RemoveIncompleteResponse(H)[1];
true
gap> P := FoldingToLineFolding(PhitoR2Fold());
[ <walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
  <walk homomorphism from a digraph with 4 edges to a digraph with 3 edges.> ]
gap> H := P[2] * PhitoR2Fold();
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> RemoveIncompleteResponse(P[1]) = RemoveIncompleteResponse(H);
true
gap> f := WalkHomomorphism(Digraph([[], []]), Digraph([[2], []]), [2, 1], []);
<walk homomorphism from a digraph with 0 edges to a digraph with 1 edge.>
gap> P := FoldingToLineFolding(f);
[ <walk homomorphism from a digraph with 0 edges to a digraph with 1 edge.>, 
  <walk homomorphism from a digraph with 0 edges to a digraph with 0 edges.> ]
gap> H := P[2] * f;
<walk homomorphism from a digraph with 0 edges to a digraph with 1 edge.>
gap> RemoveIncompleteResponse(P[1]) = RemoveIncompleteResponse(H);
true

# MaxFutureConeDepth and MaxHistoryConeDepth
gap> MaxFutureConeDepth(
> WalkHomomorphism(Digraph([]), Digraph([]), [], []));
-1
gap> MaxHistoryConeDepth(
> WalkHomomorphism(Digraph([]), Digraph([]), [], []));
-1
gap> MaxFutureConeDepth(R2toPhiFold());
0
gap> MaxHistoryConeDepth(R2toPhiFold());
0
gap> MaxFutureConeDepth(PhitoR2Fold());
0
gap> MaxHistoryConeDepth(R2toPhiFold());
0
gap> f := MakeSynchronousWalkHomomorphism(PhitoR2Fold())[1];
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> MaxFutureConeDepth(f);
0
gap> MaxHistoryConeDepth(f);
1
gap> f := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]), [1], [[1], [1]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> MaxFutureConeDepth(f);
fail
gap> MaxHistoryConeDepth(f);
fail

# ImageAsUnionOfCones
gap> ImageAsUnionOfCones(PhitoR2Fold(), 2);
[ [ [  ], 1 ] ]
gap> ImageAsUnionOfCones(PhitoR2Fold(), 1);
[ [ [  ], 1 ] ]
gap> ImageAsUnionOfCones(PhitoR2Fold(), 1);
[ [ [  ], 1 ] ]
gap> ImageAsUnionOfCones(PhitoR2Fold(), 2);
[ [ [  ], 1 ] ]
gap> ImageAsUnionOfCones(R2toPhiFold(), 1);
[ [ [  ], 1 ] ]
gap> ImageAsUnionOfCones(LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 3), 1);
[ [ [ 1, 1, 1 ], 1 ] ]
gap> ImageAsUnionOfCones(LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 3), 2);
[ [ [ 1, 1, 2 ], 1 ] ]
gap> ImageAsUnionOfCones(IdentityWalkHomomorphism(Digraph([[1, 1], []])), 1);
[ [ [  ], 1 ] ]
gap> ImageAsUnionOfCones(IdentityWalkHomomorphism(Digraph([[1, 1], []])), 2);
[ [ [  ], 2 ] ]
gap> ImageAsUnionOfCones(IdentityWalkHomomorphism(Digraph([[], [1]])), 2);
[ [ [  ], 2 ] ]

# DualWalkHomomorphism
gap> DualWalkHomomorphism(R2toPhiFold());
<walk homomorphism from a digraph with 2 edges to a digraph with 3 edges.>
gap> DualWalkHomomorphism(PhitoR2Fold());
<walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.>
gap> L12 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 2);
<walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
gap> L21 := DualWalkHomomorphism(L12);
<walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
gap> MaxFutureConeDepth(L21);
1
gap> MaxHistoryConeDepth(L21);
2

# WalksOfGivenLength
gap> WalksOfGivenLength(Digraph([[2], [1, 1]]), 3);
[ [ 1, 2, 1 ], [ 1, 3, 1 ], [ 2, 1, 2 ], [ 2, 1, 3 ], [ 3, 1, 2 ], 
  [ 3, 1, 3 ] ]
gap> WalksOfGivenLength(Digraph([[2, 2], [1, 1]]), 3);
[ [ 1, 3, 1 ], [ 1, 3, 2 ], [ 1, 4, 1 ], [ 1, 4, 2 ], [ 2, 3, 1 ], 
  [ 2, 3, 2 ], [ 2, 4, 1 ], [ 2, 4, 2 ], [ 3, 1, 3 ], [ 3, 1, 4 ], 
  [ 3, 2, 3 ], [ 3, 2, 4 ], [ 4, 1, 3 ], [ 4, 1, 4 ], [ 4, 2, 3 ], 
  [ 4, 2, 4 ] ]
gap> WalksOfGivenLength(Digraph([[1, 1]]), 3);
[ [ 1, 1, 1 ], [ 1, 1, 2 ], [ 1, 2, 1 ], [ 1, 2, 2 ], [ 2, 1, 1 ], 
  [ 2, 1, 2 ], [ 2, 2, 1 ], [ 2, 2, 2 ] ]

# LineDigraphWalkHomomorphism
gap> LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 0);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> L00 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 0);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> MaxHistoryConeDepth(L00);
0
gap> MaxFutureConeDepth(L00);
0
gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> MaxHistoryConeDepth(L01);
0
gap> MaxFutureConeDepth(L01);
1
gap> L00 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 0, 0);
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> MaxHistoryConeDepth(L00);
0
gap> MaxFutureConeDepth(L00);
0
gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 1, 0);
<walk homomorphism from a digraph with 5 edges to a digraph with 3 edges.>
gap> MaxHistoryConeDepth(L01);
0
gap> MaxFutureConeDepth(L10);
0
gap> MaxHistoryConeDepth(L10);
1
gap> L23 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 2, 3);
<walk homomorphism from a digraph with 34 edges to a digraph with 3 edges.>
gap> MaxHistoryConeDepth(L23);
2
gap> MaxFutureConeDepth(L23);
3

# TrimWalkHomomorphism
gap> W := WalkHomomorphism(Digraph([[], [1, 3], []]), Digraph([[1, 1]]),
> [1, 1, 1], [[1], [1]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> TrimWalkHomomorphism(W);
<walk homomorphism from a digraph with 0 edges to a digraph with 2 edges.>
gap> TrimWalkHomomorphism(PhitoR2Fold());
<walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.>
gap> D := Digraph([[1, 2, 2], []]);
<immutable multidigraph with 2 vertices, 3 edges>
gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [3], [2]]);
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> IsTwoSidedFolding(W);
fail
gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [2], [2]]);
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> IsTwoSidedFolding(W);                                
fail

# * and =
gap> W := WalkHomomorphism(Digraph([[], [1, 3], []]), Digraph([[1, 1]]),
> [1, 1, 1], [[1], [1]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> H := WalkHomomorphism(Digraph([[], [3, 1], []]), Digraph([[1, 1]]),
> [1, 1, 1], [[1], [1]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> W=H;
false
gap> H := WalkHomomorphism(Digraph([[], [1, 3], []]), Digraph([[1, 1]]),
> [1, 1, 1], [[1], [1]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> W=H;
true
gap> LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> L02 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 2);
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
gap> H := LineDigraphWalkHomomorphism(L01!.DomainDigraph, 0, 1);
<walk homomorphism from a digraph with 8 edges to a digraph with 4 edges.>
gap> L02 = H * L01;
true

# RemoveIncompleteResponce
gap> H := IdentityWalkHomomorphism(Digraph([[2], [1, 2]]));
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> R := RemoveIncompleteResponse(H);
[ <walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
  [ [ [ 1 ], 2 ], [ [  ], 2 ] ] ]
gap> R[1] = WalkHomomorphism(Digraph([[2], [1, 2]]), Digraph([[2], [1, 2]]),
> [2, 2], [[], [2, 1], [3]]);
true

# IsSynchronousWalkHomomorphism
gap> H := IdentityWalkHomomorphism(Digraph([[2], [1, 2]]));
<walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
gap> R := RemoveIncompleteResponse(H);
[ <walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
  [ [ [ 1 ], 2 ], [ [  ], 2 ] ] ]
gap> IsSynchronousWalkHomomorphism(H);
true
gap> IsSynchronousWalkHomomorphism(R[1]);
false

# SynchronousRemoveIncompleteResponse
gap> L40 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 4, 0);
<walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>
gap> L13 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 3);
<walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>
gap> H := SynchronousRemoveIncompleteResponse(L13);
[ <walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>,
  3 ]
gap> H[1]  = L40;
true
gap> L13 = L40;
false

#SynchronizingSequence
gap> H := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 3, 0);
<walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
gap> S := SynchronizingSequence(H);
[ <walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>,
  <walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>, 
  <walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
  <walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.> ]
gap> H := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 3, 1);
<walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>
gap> ReduceSynchronizingLength(H);
fail
gap> S[2] = LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 2, 0);
true
gap> D := Digraph([[1, 1]]);                                  
<immutable multidigraph with 1 vertex, 2 edges>
gap> D10 := LineDigraphWalkHomomorphism(D,1,0)!.DomainDigraph;
<immutable digraph with 2 vertices, 4 edges>
gap> OneSidedDigraphMinimise(D10);      
<walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
gap> D11 := LineDigraphWalkHomomorphism(D,1,1)!.DomainDigraph;
<immutable digraph with 4 vertices, 8 edges>
gap> OneSidedDigraphMinimise(D11);                            
<walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>

# WalkHomomorphismAnnotation and IsAnnotatableWalkHomomorphism
gap> H := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]),
> [1], [[1], [1, 2]]);
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> IsAnnotatableWalkHomomorphism(H);                          
false
gap> S := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]),
> [1], [[2], [1]]);   
<walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
gap> IsAnnotatableWalkHomomorphism(S);
true
gap> WalkHomomorphismAnnotation(H);
Error, AutShift: WalkHomomorphismAnnotation: usage,
the given walk homomorphism is not annotatable,
gap> WalkHomomorphismAnnotation(S);
[ 0 ]
gap> D := Digraph([[2], [2, 2, 3], [], [2]]);;
gap> H := WalkHomomorphism(D, D, [1, 2, 2, 4],
> [[1, 3], [2], [3], [3, 2], [5, 2]]);
<walk homomorphism from a digraph with 5 edges to a digraph with 5 edges.>
gap> WalkHomomorphismAnnotation(H);
[ 0, 1, 2, 0 ]
gap> WalkHomomorphismAnnotation(H, 3);
[ 3, 4, 5, 3 ]
gap> WalkHomomorphismAnnotation(H, 2, 3);
[ 0, 1, 2, 0 ]

#OneSidedDigraphMinimise