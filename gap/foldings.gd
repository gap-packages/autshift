#############################################################################
##
#W  #Y  Copyright (C) 2022                               Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declarations of the representation of a transducer and
# that of objects that relate to this package. The appropiate ViewObj functions
# are defined in the transducer.gi file.
#  g makedoc.g
# <Example>gap> 
#<walk homomorphism from a digraph with 2 edges to a digraph with 3 edges.>
#gap> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1],
#> [[1], [3, 2]]);
#Error, AutShift: WalkHomomorphism: usage,
#the walks in the forth input must be compatible with
# the vertices in the third input, </Example>


DeclareRepresentation("IsWalkHomomorphism", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["DomainDigraph",
                                              "CoDomainDigraph",
                                              "VertexMap",
                                              "EdgeMap"]);
#! @Arguments Dom, CoDom, VertexMap, EdgeMap 
#! @Returns a walk homomorphism
#! @Description
#!  Creates an object called a walk homomorphism. A walk homomorphism is 
#!  is a generalisation of a digraph homomorphism which allows mapping edges
#!  to finite walks.
#!
#!  <A>Dom</A> and <A>CoDom</A> are both digraphs (https://www.gap-system.org/Packages/digraphs.html)
#!  <A>VertexMap</A> is a list of vertices of CoDom with one entry for each vertex of
#!  <A>Dom</A>. This is to be thought of as a function from the vertices of Dom to the 
#!  vertices of <A>CoDom</A>. <A>EdgeMap</A> is a list of lists of edges of <A>CoDom</A>, each list
#!  of edges must be a walk in <A>CoDom</A> (this means each edge must end with the 
#!  start vertex of the next edge). This is to be thought of as a map from the 
#!  edges of <A>Dom</A> to the walks in <A>CoDom</A> where each edge of a digraph <A>D</A> is 
#!  identified by its position in the list <A>DigraphEdges(D)</A>;
# <A>conv</A>.
#! <Example>gap> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1], [[1], [2, 3]]);
#! &lt;walk homomorphism from a digraph with 2 edges to a digraph with 3 edges.&gt; </Example>
DeclareOperation("WalkHomomorphism", [IsDigraph, IsDigraph, IsDenseList, IsDenseList]);

#! @Arguments W 
#! @Returns true or false
#! @Description
#!  A walk homomorphism is called degenerate if there is a biinfinite walk through
#!  its domain which is not mapped by <A>W</A> to a biinfinite walk in its codomain.
#!  Equivalently it is degenerate if there is a finite non-empty walk in the domain 
#!  with the same start and end points which is mapped by <A>W</A> to an empty walk.
#!
#!  This attribute returns true if and only if <A>W</A> is degenerate.
# <A>conv</A>.
#! <Example> 
#!gap> IsDegenerateWalkHomomorphism(PhitoR2Fold());
#!false
#!gap> IsDegenerateWalkHomomorphism(
#!> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1]]), [1], [[1], []]));
#!true
#!gap> IsDegenerateWalkHomomorphism(
#!> WalkHomomorphism(Digraph([[2], [1]]), Digraph([[1]]), [1, 1], [[], []]));
#!true </Example>
DeclareAttribute("IsDegenerateWalkHomomorphism", IsWalkHomomorphism);

#! @Arguments W 
#! @Returns a pair of walk homomorphisms
#! @Description
#!  A walk homomorphism is called synchronous if it maps each edge to a walk of length 1.
#!  Hence synchronous walk homomorphisms as essentially the same as digraph 
#!  homomorphisms.
#!
#!  If <A>W</A> is an UDAF folding between digraphs D1 and D2, then 
#!  this operation returns a synchronous UDAF folding H from some digraph D3 to D2
#!  and an UDAF folding f from D3-> D1 such that H and the composite fW induce 
#!  the same UDAF isomorphism.
#! <Example> 
#!gap> MakeSynchronousWalkHomomorphism(
#!> WalkHomomorphism(Digraph([[]]), Digraph([[]]), [1], []));
#![ &lt;walk homomorphism from a digraph with 0 edges to a digraph with 0 edges.>, 
#!  &lt;walk homomorphism from a digraph with 0 edges to a digraph with 0 edges.> ]
#!gap> MakeSynchronousWalkHomomorphism(
#!> WalkHomomorphism(Digraph([[1]]), Digraph([[]]), [1], [[]]));
#!Error, AutShift: MakeSynchronousWalkHomomorphism: usage,
#!the given homomorphism must be non-degenerate
#!gap> MakeSynchronousWalkHomomorphism(
#!> WalkHomomorphism(Digraph([[]]), Digraph([[1]]), [1], []));
#!Error, AutShift: MakeSynchronousWalkHomomorphism: usage,
#!the target digraph must be an UDAF Digraph
#!gap> P := MakeSynchronousWalkHomomorphism(PhitoR2Fold());
#![ &lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
#!  &lt;walk homomorphism from a digraph with 4 edges to a digraph with 3 edges.> ]
#!gap> IsSynchronousWalkHomomorphism(P[1]);
#!true
#!gap> H := P[2] * PhitoR2Fold();
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> RemoveIncompleteResponse(H)[1] = RemoveIncompleteResponse(P[1])[1];
#!true
#!true </Example>
DeclareOperation("MakeSynchronousWalkHomomorphism", [IsWalkHomomorphism]);


#! @Arguments  
#! @Returns A walk homomorphism
#! @Description
#!  This returns the same output as WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1], [[1], [2, 3]]);
DeclareOperation("R2toPhiFold", []);

#! @Arguments  
#! @Returns A walk homomorphism
#! @Description
#!  This returns the same output as WalkHomomorphism(Digraph([[1, 2], [1]]), Digraph([[1, 1]]), [1, 1], [[1], [2], []]]);
DeclareOperation("PhitoR2Fold", []);


#! @Arguments W 
#! @Returns an automaton
#! @Description
#!  This returns an automaton A (https://www.gap-system.org/Packages/automata.html)
#!  such that for each vertex in the domain of <A>W</A>, the regular language of
#!  words which can be read from the corresponding state of A is equal to the 
#!  set of walks in the image of the vertex.
#!  
#!  Here by "image of the vertex" we mean the set of finite prefixes of walks in 
#!  the codomain of <A>W</A> which can be obtained by applying <A>W</A> to 
#!  forwards walk in the domain of <A>W</A> starting with the given vertex.
#!
# <A>conv</A>.
#! <Example> 
#!gap> WalkHomomorphismVertexImageAutomaton(R2toPhiFold(), 1);
#!&lt; epsilon automaton on 4 letters with 2 states >
#!gap> WalkHomomorphismVertexImageAutomaton(PhitoR2Fold(), 1);
#!&lt; epsilon automaton on 3 letters with 2 states >
#!gap> WalkHomomorphismVertexImageAutomaton(PhitoR2Fold(), 2);
#!&lt; epsilon automaton on 3 letters with 2 states >
#!</Example>
DeclareAttribute("WalkHomomorphismImageAutomaton", IsWalkHomomorphism);

#! @Arguments W 
#! @Returns an automaton
#! @Description
#!  This returns the same automaton as WalkHomomorphismImageAutomaton but with its
#!  initial state set to the specified vertex
DeclareOperation("WalkHomomorphismVertexImageAutomaton", [IsWalkHomomorphism, IsInt]);

#! @Arguments W 
#! @Returns a walk homomorphism
#! @Description
#!  This returns an walk homomorphism W2 such that each vertex in the domain of W
#!  has the same image as the corresponding vertex of the domain of W2.
#!  W2 has the additional properties that each edge is mapped to a walk of
#!  length one, and no two edges with the same start vertex are mapped
#!  to the same walk of length 1. This is done in a manner analogous to the
#!  "power set construction" for regular languages.
#!
#!  As some vertices in the domain of <A>W</A> may have the same image, we do not
#!  combine the "equivalent states" of W2 so that the above condition remains
#!  true. If the user wants a smaller output walk homomorphism they may be interested
#!  in the operation ImageFinderWalkHomomorphism instead.
#!
#!  Here by "image of the vertex" we mean the set of finite prefixes of walks in 
#!  the codomain of <A>W</A> which can be obtained by applying <A>W</A> to 
#!  forwards walk in the domain of <A>W</A> starting with the given vertex.
#!
# <A>conv</A>.
#! <Example> 
#!gap> W := WalkHomomorphism(Digraph([[2, 2], [2, 2]]), Digraph([[1, 1]]), [1, 1], [[2, 1], [2, 1], [1], [2]]);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> ImageAsUnionOfCones(W, 1);                                                                              
#![ [ [ 2, 1 ], 1 ] ]
#!gap> W2 := PowerSetWalkHomomorphism(W);                                                                      
#!&lt;walk homomorphism from a digraph with 6 edges to a digraph with 2 edges.>
#!gap> ImageAsUnionOfCones(W2, 1);       
#![ [ [ 2, 1 ], 1 ] ]
#!</Example>
DeclareOperation("PowerSetWalkHomomorphism", [IsWalkHomomorphism]);

#! @Arguments W 
#! @Returns a walk homomorphism and a list of integers
#! @Description
#!  This returns an walk homomorphism W2 and a list of vertices of W2.
#!  This list of vertices has an entry for each vertex in the domain of W, and 
#!  assigns each vertex a vertex in the domain of W2 with the same image.
#!
#!  As is the case with PowerSetWalkHomomorphism W2 has the additional properties
#!  that each edge is mapped to a walk of length one, and no two edges with the
#!  same start vertex are mapped to the same walk of length 1. 
#!
#!  W2 now has the additional property that no to vertices in the domain of W2
#!  have the same image (hense the need for the vertex list).
#!
#!  Here by "image of the vertex" we mean the set of finite prefixes of walks in 
#!  the codomain of <A>W</A> which can be obtained by applying <A>W</A> to 
#!  forwards walk in the domain of <A>W</A> starting with the given vertex.
#!
# <A>conv</A>.
#! <Example> 
#!gap> W := WalkHomomorphism(Digraph([[2, 2], [2, 2]]), Digraph([[1, 1]]), [1, 1], [[2, 1], [2, 1], [1], [2]]);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> ImageAsUnionOfCones(W, 1);
#![ [ [ 2, 1 ], 1 ] ]
#!gap> W2 := ImageFinderWalkHomomorphism(W);
#![ &lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, [ 3, 2 ] ]
#!gap> ImageAsUnionOfCones(W2[1], 3);                                                                          
#![ [ [ 2, 1 ], 1 ] ]
#!</Example>
DeclareAttribute("ImageFinderWalkHomomorphism", IsWalkHomomorphism);

#! @Arguments W 
#! @Returns a walk homomorphism
#! @Description
#!  The Dual digraph of a digraph is the digraph obtained by "reversing the arrows".
#!  That is replacing each edge with a new edge which starts where the previous one
#!  ended and ends where the previous one started. Note that this completely reoders
#!  the edges.
#!
#!  If one applies this process to the domain and codomain of a walk homomorhism 
#!  one naturally obtains a new walk homomorphism called its dual walk homomrphism.
#!  This Attribute returns a dual of <A>W</A>.
#!
#!  The dual while unique up to "isomorphism" is not unique due to the arbitary
#!  decisions made when reordering certain edges. So there is no guarentee that
#!  the this atribute will undo itself.
#!
#! <Example> 
#!gap> L12 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 2);
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> D := DualWalkHomomorphism(L21);
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> D2 := DualWalkHomomorphism(D);
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> D2 = L12;
#!false
#!gap> OutNeighbours(D2!.DomainDigraph); 
#![ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ], [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ] ]
#!gap> OutNeighbours(L12!.DomainDigraph);
#![ [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ], [ 1, 2 ], [ 3, 4 ], [ 5, 6 ], [ 7, 8 ] ]
#!gap> D3 := DualWalkHomomorphism(D2);  
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> D3 = D; 
#!true
#!</Example>
DeclareAttribute("DualWalkHomomorphism", IsWalkHomomorphism);

#! @Arguments D, n
#! @Returns a list of lists of integers
#! @Description
#!  Returns all the walks in <A>D</A> of length <A>n</A>. Each walk is given as
#!  a sequence of edges. If <A>n</A> is 0 then the operation returns fail.
#!
#! <Example> 
#!gap> WalksOfGivenLength(Digraph([[2], [1, 1]]), 3);
#![ [ 1, 2, 1 ], [ 1, 3, 1 ], [ 2, 1, 2 ], [ 2, 1, 3 ], [ 3, 1, 2 ], 
#!  [ 3, 1, 3 ] ]
#!gap> WalksOfGivenLength(Digraph([[2, 2], [1, 1]]), 3);
#![ [ 1, 3, 1 ], [ 1, 3, 2 ], [ 1, 4, 1 ], [ 1, 4, 2 ], [ 2, 3, 1 ], 
#!  [ 2, 3, 2 ], [ 2, 4, 1 ], [ 2, 4, 2 ], [ 3, 1, 3 ], [ 3, 1, 4 ], 
#!  [ 3, 2, 3 ], [ 3, 2, 4 ], [ 4, 1, 3 ], [ 4, 1, 4 ], [ 4, 2, 3 ], 
#!  [ 4, 2, 4 ] ]
#!gap> WalksOfGivenLength(Digraph([[1, 1]]), 3);
#![ [ 1, 1, 1 ], [ 1, 1, 2 ], [ 1, 2, 1 ], [ 1, 2, 2 ], [ 2, 1, 1 ], 
#!  [ 2, 1, 2 ], [ 2, 2, 1 ], [ 2, 2, 2 ] ]
#!</Example>
DeclareOperation("WalksOfGivenLength", [IsDigraph, IsInt]);

#! @Arguments D
#! @Returns a list of lists of pairs of integers
#! @Description
#!  Returns a list whose vth entry is a list of pairs. There is one pair
#!  for each edge in D starting at vertex v. The pair contains the number of the
#!  edge (its position in DigraphEdges(<A>D</A>)) and the vertex it points to in
#!  that order.
#!
#! <Example> 
#!gap> OutEdgesAtVertex(D);
#![ [ [ 1, 1 ], [ 2, 1 ] ] ]
#!gap> D := Digraph([[1, 1]]);
#!&lt;immutable multidigraph with 1 vertex, 2 edges>
#!gap> OutEdgesAtVertex(D);
#![ [ [ 1, 1 ], [ 2, 1 ] ] ]
#!gap> D := Digraph([[2], [1]]);
#!&lt;immutable digraph with 2 vertices, 2 edges>
#!gap> OutEdgesAtVertex(D);     
#![ [ [ 1, 2 ] ], [ [ 2, 1 ] ] ]
#!gap> D := Digraph([[2], [1], [1]]);
#!&lt;immutable digraph with 3 vertices, 3 edges>
#!gap> OutEdgesAtVertex(D);          
#![ [ [ 1, 2 ] ], [ [ 2, 1 ] ], [ [ 3, 1 ] ] ]
#!</Example>
DeclareAttribute("OutEdgesAtVertex", IsDigraph);

#! @Arguments D
#! @Returns a walk homomorphisms
#! @Description
#!  Returns the walk homomorphism from <A>D</A> to <A>D</A> which maps each edge
#!  to the walk of length 1 containing it, and fixing all vertices.
#!
#! <Example> 
#!gap> H := IdentityWalkHomomorphism(Digraph([[2], [1, 2]]));
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!</Example>
DeclareOperation("IdentityWalkHomomorphism", [IsDigraph]);

#! @Arguments W, v
#! @Returns a list
#! @Description
#!  A "cone" for a digraph is a set of infinite forwards walks in the digraph which is equal to
#!  the set of all infinite forwards walks which start with a fixed finite walk.
#!
#!  If the image of v can be expressed as a union of cones then the operation
#!  returns a list of cones whose union is the image of v, if not then the operation
#!  returns fail. A cone is given as a pair, whose first entry is the edge walk
#!  shared by the elements of the cone, and whose second entry is the vertex at
#!  the end of the edge walk (thus we can distingish between empty walks at 
#!  different vertices).
#!  In the event that there are multiple finite walks defining a cone, the shortest
#!  one is given.
#!
#!  Here by "image of v" we mean the set of infinite walks in 
#!  the codomain of <A>W</A> which can be obtained by applying <A>W</A> to 
#!  forwards infinite walk in the domain of <A>W</A> starting with the given 
#!  vertex.
#! <Example> 
#!gap> W := WalkHomomorphism(Digraph([[2, 2], [2, 2]]), Digraph([[1, 1]]), [1, 1], [[2, 1], [2, 1], [1], [2]]);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> ImageAsUnionOfCones(W, 1);                                                                              
#![ [ [ 2, 1 ], 1 ] ]
#!gap> W := WalkHomomorphism(Digraph([[2, 2], [2, 2, 2]]), Digraph([[1, 1, 1]]), [1, 1], [[1], [2], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 3 edges.>
#!gap> ImageAsUnionOfCones(W, 1);
#![ [ [ 1 ], 1 ], [ [ 2 ], 1 ] ]
#!gap> W := WalkHomomorphism(Digraph([[2, 2], [2, 2, 2]]), Digraph([[1, 1, 1, 1]]), [1, 1], [[1], [2], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 4 edges.>
#!gap> ImageAsUnionOfCones(W, 1);                                                                                      
#!fail
#!</Example>
DeclareOperation("ImageAsUnionOfCones", [IsWalkHomomorphism, IsInt]);

#! @Arguments W
#! @Returns a list
#! @Description
#!  This attribute outputs the list whose vth entry is ImageAsUnionOfCones(<A>W</A>, v).
DeclareAttribute("ImagesAsUnionsOfCones", IsWalkHomomorphism);

#! @Arguments A, B
#! @Returns a walk homomorphism
#! @Description
#!  If <A>A</A> is a walk homomorphism from D1 to D2 and <A>B</A> is a walk 
#!  homomorphism from D2 to D3, then the composite of <A>A</A> and <B>B</B> is
#!  a walk homomorphism from D1 to D3. The vertex map is the composite of
#!  the vertex maps of <A>A</A> and <A>B</A>. The edge map maps an edge
#!  e to the walk obtained by applying <A>A</A> to e, and concatening the walks
#!  obtained by applying <A>B</A> to the edges in this walk.
#!
#!  This operation returns the composite of <A>A</A> and <A>B</A> if they are
#!  composable and returns fail otherwise.
#!
#!  This operation and also be called using *.
#! <Example> 
#!gap> I1 := IdentityWalkHomomorphism(Digraph([[1, 1, 1]]));
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> I2 := IdentityWalkHomomorphism(Digraph([[1, 1]]));
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> I1*I2;
#!fail
#!gap> R2toPhiFold() * PhitoR2Fold();
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!</Example>
DeclareOperation("ComposeWalkHomomorphisms", [IsWalkHomomorphism, IsWalkHomomorphism]);

#! @Arguments D, p, f
#! @Returns a walk homomorphism
#! @Description
#!  If <A>D</A> is a digraph then one can natually contruct a new digraph from 
#!  <A>D</A> called its line digraph which has a vertex for each walk of length
#!  1 and an edge for each walk of length 2 see https://en.wikipedia.org/wiki/Line_graph. 
#!  
#!  There are two natural digraph hommorphisms from this new digraph to the old
#!  one. Defined by mapping each vertex to its start or end vertex in the 
#!  origional digraph. These homomorphisms are the output of 
#!  LineDigraphWalkHomomorphism(<A>D</A>, 0, 1) and LineDigraphWalkHomomorphism(<A>D</A>, 1, 0)
#!  respectively. The idea being that the vertices in the former digraph store
#!  one edge of future information and those in the latter store one edge of
#!  past information.
#!
#!  A vertex of the domain of LineDigraphWalkHomomorphism(<A>D</A>, p, f) is
#!  is a walk of length p + f and the vertex map sends such a vertex to the
#!  vertex in the origional digraph which is p edges from the front and f
#!  edges from the end.
#!
#!  This construction is nice in the sense that if a, b, c, d are positive integers
#!  and D2 is the doman of LineDigraphWalkHomomorphism(<A>D</A>, a, b), then
#!  LineDigraphWalkHomomorphism(<A>D2</A>, c, d) * LineDigraphWalkHomomorphism(<A>D</A>, a, b)
#!  agrees with LineDigraphWalkHomomorphism(<A>D</A>, a + c, b + d).
#! <Example>
#!gap> L12 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 1, 2);
#!&lt;walk homomorphism from a digraph with 13 edges to a digraph with 3 edges.>
#!gap> N11 := LineDigraphWalkHomomorphism(L12!.DomainDigraph, 1, 1);    
#!&lt;walk homomorphism from a digraph with 34 edges to a digraph with 13 edges.>
#!gap> L23 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 2, 3);
#!&lt;walk homomorphism from a digraph with 34 edges to a digraph with 3 edges.>
#!gap> N11 * L12 = L23;
#!true
#!</Example>
DeclareOperation("LineDigraphWalkHomomorphism", [IsDigraph, IsInt, IsInt]);

#! @Arguments D
#! @Returns true or false
#! @Description
#!  If <A>D</A> is a digraph, then we say that <A>D</A> is an UDAF digraph
#!  if for all vertices v of <A>D</A> we have that neither the number of infinite
#!  backwards walks ending at <A>D</A> nor the number of infinite forwards walks
#!  begining at v is equal to 1.
#!
#!  This property is to ensure that the "irrational" walks in <A>D</A> are "dense"
#!  this property is desirable as is allows us to prove various facts about walk
#!  homomorphisms between these digraphs see the paper https://arxiv.org/abs/2112.13359
#!  for more details.
#!
#!  Moreover some of the operations in this package will reject digraphs that are
#!  are not UDAF digraphs as it is not known that they will work as inteded in such
#!  cases.
#! <Example>
#!gap> IsUDAFDigraph(Digraph([[1, 1]]));
#!true
#!gap> IsUDAFDigraph(Digraph([[1]]));
#!false
#!gap> IsUDAFDigraph(Digraph([[], []]));
#!true
#!gap> IsUDAFDigraph(Digraph([[2], [1]]));
#!false
#!gap> IsUDAFDigraph(Digraph([[1, 2], [1]]));
#!true
#!gap> IsUDAFDigraph(Digraph([[1, 1], [1]]));
#!true
#!gap> IsUDAFDigraph(Digraph([[2, 2], [2]]));
#!false
#!gap> IsUDAFDigraph(Digraph([[2], [2, 2]]));
#!true
#!gap> D := IsUDAFDigraph(Digraph([[1, 1, 2], [3], []]));
#!true
#!gap> D := IsUDAFDigraph(Digraph([[1, 1, 2], [3], [2]]));
#!false
#!</Example>
DeclareAttribute("IsUDAFDigraph", IsDigraph);

#! @Arguments W
#! @Returns true or false
#! @Description
#!  A walk homomorphism is called an UDAF folding if it induces a bijection
#!  between the sets of shist equivalence clasees of biinfinite walks of the
#!  domain and codomain (which we require to be UDAF digraphs).
#! <Example>
#!gap> IsUDAFFolding(R2toPhiFold());
#!true
#!gap> IsUDAFFolding(PhitoR2Fold());
#!true
#!gap> IsUDAFFolding(IdentityWalkHomomorphism(Digraph([[], [1,3], []])));
#!true
#!gap> IsUDAFFolding(IdentityWalkHomomorphism(Digraph([[2], [1,3], []])));
#!false
#!gap> IsUDAFFolding(IdentityWalkHomomorphism(Digraph([[2], [1,2, 3], []])));
#!true
#!gap> IsUDAFFolding(
#!> WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]), [1], [[1], []]));
#!false
#!gap> IsUDAFFolding(
#!> WalkHomomorphism(Digraph([[1, 1], [2, 2]]), Digraph([[1, 1]]),
#!> [1, 1], [[1], [2], [1], [2]]));
#!false
#!gap> IsUDAFFolding(
#!> WalkHomomorphism(Digraph([[1, 1, 1]]), Digraph([[1, 1]]),
#!> [1], [[1], [2], [1]]));
#!false
#!gap> f:= WalkHomomorphism(Digraph([ [ 1, 2, 3 ], [ 1, 4, 1 ], [ 1, 2, 3 ], 
#!> [ 1, 4, 1 ] ]), Digraph([ [ 3 ], [ 3 ], [ 3, 3, 3 ] ]), [ 3, 3, 3, 3 ], 
#!> [ [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], [ 4 ], [ 5 ], [ 3 ], 
#!> [ 4 ], [ 5 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 5 edges.>
#!gap> IsUDAFFolding(f);
#!true
#!</Example>
DeclareAttribute("IsUDAFFolding", IsWalkHomomorphism);

#! @Arguments W
#! @Returns a walk homomorphism
#! @Description
#!  A walk homomorphism is called trimmed if every vertex (and hense edge) of its
#!  domain is involved in a biinfinite walk.
#!
#!  This Operation returns the walk homomorphism obtained from W by restricting 
#!  the domain of W to the subdigraph of the domain including precisely those 
#!  vertices and edges which are involved in biinfinite walks.
#! <Example>
#!gap> W := WalkHomomorphism(Digraph([[], [1, 3], []]), Digraph([[1, 1]]),
#!> [1, 1, 1], [[1], [1]]);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> TrimWalkHomomorphism(W);
#!&lt;walk homomorphism from a digraph with 0 edges to a digraph with 2 edges.>
#!gap> TrimWalkHomomorphism(PhitoR2Fold());
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.>
#!</Example>
DeclareOperation("TrimWalkHomomorphism", [IsWalkHomomorphism]);

#! @Arguments W
#! @Returns an integer
#! @Description
#!  This attribute is primarily intended to be used with UDAF foldings. If W
#!  is an UDAF folding in which the image of every vertex of can be expressed
#!  as a union of cones, this attribute returns the longest of length of a 
#!  finite walk required to define these cones. if W is not of this type then
#!  the attribute return fail, and if all vertices have empty image, then the
#!  attribute returns -1.
#!
#!  A "cone" for a digraph is a set of infinite forwards walks in the digraph which is equal to
#!  the set of all infinite forwards walks which start with a fixed finite walk.
#!
#!  Here by image of a vertex we mean the set of infinite walks in 
#!  the codomain of <A>W</A> which can be obtained by applying <A>W</A> to 
#!  forwards infinite walk in the domain of <A>W</A> starting with the given 
#!  vertex.
#!
#! <Example> 
#!gap> L00 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 0);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> MaxFutureConeDepth(L00);
#!0
#!gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> MaxFutureConeDepth(L01);
#!1
#!gap> L00 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 0, 0);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> MaxFutureConeDepth(L00);
#!0
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 1, 0);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 3 edges.>
#!gap> MaxFutureConeDepth(L10);
#!0
#!gap> L23 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 2, 3);
#!&lt;walk homomorphism from a digraph with 34 edges to a digraph with 3 edges.>
#!gap> MaxFutureConeDepth(L23);
#!3
#!</Example>
DeclareAttribute("MaxFutureConeDepth", IsWalkHomomorphism);

#! @Arguments W
#! @Returns an integer
#! @Description
#!  This attribute is primarily intended to be used with UDAF foldings. If W
#!  is an UDAF folding in which the backwards image of every vertex of can be expressed
#!  as a union of backwards cones, this attribute returns the longest of length of a 
#!  finite walk required to define these cones. if W is not of this type then
#!  the attribute return fail, and if all vertices have empty backwards image, then the
#!  attribute returns -1.
#!
#!  A "backwards cone" for a digraph is a set of infinite backwards walks in the digraph which is equal to
#!  the set of all infinite backwards walks which end with a fixed finite walk.
#!
#!  Here by backwards image of a vertex we mean the set of infinite walks in 
#!  the codomain of <A>W</A> which can be obtained by applying <A>W</A> to 
#!  backwards infinite walk in the domain of <A>W</A> ending with the given 
#!  vertex.
#!
#! <Example> 
#!gap> L00 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 0);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> MaxHistoryConeDepth(L00);
#!0
#!gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> MaxHistoryConeDepth(L01);
#!0
#!gap> L00 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 0, 0);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> MaxHistoryConeDepth(L00);
#!0
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 1, 0);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 3 edges.>
#!gap> MaxHistoryConeDepth(L01);
#!0
#!gap> MaxHistoryConeDepth(L10);
#!1
#!gap> L23 := LineDigraphWalkHomomorphism(Digraph([[1, 2], [1]]), 2, 3);
#!&lt;walk homomorphism from a digraph with 34 edges to a digraph with 3 edges.>
#!gap> MaxHistoryConeDepth(L23);
#!2
#!</Example>
DeclareAttribute("MaxHistoryConeDepth", IsWalkHomomorphism);

#! @Arguments W
#! @Returns true or false
#! @Description
#!  We say that a walk homomorphism <A>W</A> is deterministic if and only if for all
#!  vertices v in the domain of <A>W</A>, <A>W</A> mapps the set of edges starting with
#!  v bijectively to the set of edges in the codomain of <A>W</A> which begin with the
#!  vertex which <A>W</A> maps v to.
#!
#!  In particular, deterministic walk homomorphisms are always synchronous. 
#!  The name deteministic comes from the observation that a walk homomorphism to
#!  the one vertex, n-edge digraph is deterministic if and only if the corresponding
#!  automaton is.
#!
#! <Example> 
#!gap> R2 := Digraph([[1, 1]]);                     
#!&lt;immutable multidigraph with 1 vertex, 2 edges>
#!gap> L01 := LineDigraphWalkHomomorphism(R2, 0, 1);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> L10 := LineDigraphWalkHomomorphism(R2, 1, 0);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> IsDeterministicWalkHomomorphism(L01);
#!false
#!gap> IsDeterministicWalkHomomorphism(L10);
#!true
#!gap> IsDeterministicWalkHomomorphism(R2toPhiFold());
#!false
#!gap> IsDeterministicWalkHomomorphism(PhitoR2Fold()); 
#!false
#!</Example>
DeclareAttribute("IsDeterministicWalkHomomorphism", IsWalkHomomorphism);

#! @Arguments A, B
#! @Returns a walk homomorphism
#! @Description
#!  Same as ComposeWalkHomomorphisms.
#! <Example> 
#!gap> I1 := IdentityWalkHomomorphism(Digraph([[1, 1, 1]]));
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> I2 := IdentityWalkHomomorphism(Digraph([[1, 1]]));
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> I1*I2;
#!fail
#!gap> R2toPhiFold() * PhitoR2Fold();
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!</Example>
DeclareOperation("\*", [IsWalkHomomorphism, IsWalkHomomorphism]);

#! @Arguments W
#! @Returns A list
#! @Description
#!  We say that a walk homomorphism <A>W</A> has incomplete responce if there is a
#!  vertex in its domain such that all edges originating at the vertec are mapped
#!  to walks with a common initial edge. This is based on the concept of the same
#!  name from: Grigorchuk, R. I., Nekrashevych, V. V., and Sushchansky, V. I. (2000). 
#!  Automata, dynamical systems, and groups. Trudy Matematicheskogo Instituta 
#!  Imeni VA Steklova, 231, 134-214.
#!
#!  We require the given walk homomorphism to map to an UDAF Digraph and for
#!  the image of each vertex to be a union of cones.
#!
#!  This operation replaces <A>W</A> with a new walk homomorphism by first trimming
#!  (see TrimWalkHomomorphism) <A>W</A> and then mapping each edge in the domain
#!  of <A>W</A> to the walk obtained by starting with the walk it is currently 
#!  mapped to, and then:
#!  1. appending the longest common prefix of the walks in the image of the 
#!  target vertex.
#!  2. removing the longest common prefix of the walks in the image of the 
#!  source vertex.
#!  The vertex map is then defined in the unique possible fashion.
#!
#!  The first entry of the output is the new walk homomorphism. The second
#!  entry is the list of prefixes that where removed from the starts of the
#!  walks at each vertex (given as a pair consisting of the edge sequence and the
#!  ending vertex).
#!
#!  This operation is useful because of the following two observations:
#!  1. removing incomplete responce from a trimmed UDAF folding doesn't change 
#!  the induced UDAF isomorphism.
#!  2. If f1, f2:D1 to D2 are trimmed UDAF foldings with complete responce which 
#!  induce the same UDAF isomorphism then f1 = f2.
#! <Example> 
#!gap> H := IdentityWalkHomomorphism(Digraph([[2], [1, 2]]));
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> R := RemoveIncompleteResponse(H);
#![ &lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
#!  [ [ [ 1 ], 2 ], [ [  ], 2 ] ] ]
#!gap> R[1] = WalkHomomorphism(Digraph([[2], [1, 2]]), Digraph([[2], [1, 2]]),
#!> [2, 2], [[], [2, 1], [3]]);
#!true
#!</Example>
DeclareOperation("RemoveIncompleteResponse", [IsWalkHomomorphism]);

#! @Arguments W 
#! @Returns true or false
#! @Description
#!  A walk homomorphism is called synchronous if it maps each edge to a walk of length 1.
#!  Hence synchronous walk homomorphisms as essentially the same as digraph 
#!  homomorphisms.
#!
#!  This attribute returns true if and only if <A>W</A> is syncrhonous.
# <A>conv</A>.
#! <Example> 
#!gap> IsSynchronousWalkHomomorphism(PhitoR2Fold());
#!false
#!gap> h := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]), [1], [[2], [2]]);;
#!gap> IsSynchronousWalkHomomorphism(h);
#!true </Example>
DeclareAttribute("IsSynchronousWalkHomomorphism", IsWalkHomomorphism);

#! @Arguments W
#! @Returns A list
#! @Description
#! This operation functions similarly to RemoveIncompleteResponse with the difference
#! being that it assumed that the input folding is syncrhonous and incomplete
#! will only be removed as much as possible which remaining synchronousness
#! of the walk homomorphism.
#!
#! As a result the second ouptut instead of a list of removed prefixes simply
#! gives an integer corresponding to the amount of incomplete remose removed
#! from each vertex.
#!
#! <Example> 
#!gap> L40 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 4, 0);
#!&lt;walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>
#!gap> L13 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 3);
#!&lt;walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>
#!gap> H := SynchronousRemoveIncompleteResponse(L13);
#![ &lt;walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>,
#!  3 ]
#!gap> H[1]  = L40;
#!true
#!gap> L13 = L40;
#!false
#!</Example>
DeclareOperation("SynchronousRemoveIncompleteResponse", [IsWalkHomomorphism]);

#! @Arguments W
#! @Returns true or false
#! @Description
#! An annotation for a walk homomorphism <A>W</A> is a function A from the vertex
#! set of the domain of <A>W</A> to the integers with the propery that for all
#! edges e in the domain of <A>W</A> from a vertex a to a vertex b, the length
#! L of the walk <A>W</A> maps e to satisfies (b)A - (a)A + 1 = L.
#!
#! Annotations are useful in using walk homomorphism to define continuous maps
#! between shift space. In particular a constant annotation is always valid for
#! synchronous walk homomorphisms.
#!
#! This attribute returns true if and only if the given walk homomorphism admits
#! an annotation.
#! <Example> 
#!gap> H := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]),
#!> [1], [[1], [1, 2]]);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> IsAnnotatableWalkHomomorphism(H);                          
#!false
#!gap> S := WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 1]]),
#!> [1], [[2], [1]]);   
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> IsAnnotatableWalkHomomorphism(S);
#!true
#!</Example>
DeclareAttribute("IsAnnotatableWalkHomomorphism", IsWalkHomomorphism);

#! @Arguments W, s, p
#! @Returns A list of integers
#! @Description
#! An annotation for a walk homomorphism <A>W</A> is a function A from the vertex
#! set of the domain of <A>W</A> to the integers with the propery that for all
#! edges e in the domain of <A>W</A> from a vertex a to a vertex b, the length
#! l of the walk <A>W</A> maps e to satisfies (b)A - (a)A + 1 = l.
#!
#! Annotations are useful in using walk homomorphism to define continuous maps
#! between shift space. In particular a contstant annotation is always valid for
#! synchronous walk homomorphisms.
#!
#! If <A>>W</A> is a walk homomorphism which admits an annotation, then this
#! operation returns an annotation for <A>W</A> such that the vertex s in the
#! domain of <A>W</A> is mapped to p. This is given as a list of integers.
#! <Example> 
#!gap> D := Digraph([[2], [2, 2, 3], [], [2]]);;
#!gap> H := WalkHomomorphism(D, D, [1, 2, 2, 4],
#!> [[1, 3], [2], [3], [3, 2], [5, 2]]);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 5 edges.>
#!gap> WalkHomomorphismAnnotation(H);
#![ 0, 1, 2, 0 ]
#!gap> WalkHomomorphismAnnotation(H, 3);
#![ 3, 4, 5, 3 ]
#!gap> WalkHomomorphismAnnotation(H, 2, 3);
#![ 0, 1, 2, 0 ]
#!</Example>
DeclareOperation("WalkHomomorphismAnnotation", [IsWalkHomomorphism, IsInt, IsInt]);
#! @Arguments W, p
#! @Returns A list of integers
#! @Description
#! Same as WalkHomomorphismAnnotation(1, p).
#! <Example> 
#!gap> D := Digraph([[2], [2, 2, 3], [], [2]]);;
#!gap> H := WalkHomomorphism(D, D, [1, 2, 2, 4],
#!> [[1, 3], [2], [3], [3, 2], [5, 2]]);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 5 edges.>
#!gap> WalkHomomorphismAnnotation(H);
#![ 0, 1, 2, 0 ]
#!gap> WalkHomomorphismAnnotation(H, 3);
#![ 3, 4, 5, 3 ]
#!gap> WalkHomomorphismAnnotation(H, 2, 3);
#![ 0, 1, 2, 0 ]
#!</Example>
DeclareOperation("WalkHomomorphismAnnotation", [IsWalkHomomorphism, IsInt]);

#! @Arguments W
#! @Returns A list of integers
#! @Description
#! Same as WalkHomomorphismAnnotation(1, 0).
#! <Example> 
#!gap> D := Digraph([[2], [2, 2, 3], [], [2]]);;
#!gap> H := WalkHomomorphism(D, D, [1, 2, 2, 4],
#!> [[1, 3], [2], [3], [3, 2], [5, 2]]);
#!&lt;walk homomorphism from a digraph with 5 edges to a digraph with 5 edges.>
#!gap> WalkHomomorphismAnnotation(H);
#![ 0, 1, 2, 0 ]
#!gap> WalkHomomorphismAnnotation(H, 3);
#![ 3, 4, 5, 3 ]
#!gap> WalkHomomorphismAnnotation(H, 2, 3);
#![ 0, 1, 2, 0 ]
#!</Example>
DeclareOperation("WalkHomomorphismAnnotation", [IsWalkHomomorphism]);

#! @Arguments W
#! @Returns true or false
#! @Description
#! We say that a walk homomorphism is a one-sided folding if it is sycnhronous and
#! itinduces a bijection beween the set of infinite backwards walks of the domain and
#! codomain digraphs.
#!
#! This attribute returns true if and only if the given walk homomorphism is a
#! one-sided folding.
#! <Example> 
#!gap> H := WalkHomomorphism(Digraph([[1, 1]]),Digraph([[1,1]]),[1],[[2],[1]]);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> I := IdentityWalkHomomorphism(Digraph([[1, 1]]));
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> IsOneSidedFolding(H);         
#!true
#!gap> IsOneSidedFolding(I);                                                   
#!true
#!gap> H := WalkHomomorphism(Digraph([[1, 1]]),Digraph([[1,1]]),[1],[[2],[2]]);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> IsOneSidedFolding(H);
#!false
#!gap> D := Digraph([[1, 2, 2], []]);
#!&lt;immutable multidigraph with 2 vertices, 3 edges>
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [3], [2]]);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> IsOneSidedFolding(W);
#!true
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [2], [2]]);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> IsOneSidedFolding(W);                                
#!false
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [1, 3], [2]]);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> IsOneSidedFolding(W);                                   
#!false
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[], [1, 3], [2]]); 
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> IsOneSidedFolding(W);                                  
#!false
#!</Example>
DeclareAttribute("IsOneSidedFolding", IsWalkHomomorphism);

#! @Arguments W
#! @Returns true or false
#! @Description
#! It is assummed that the given walk homomorphisms are between UDAF digraphs
#! and the operation will return fail if this is not the case.
#!
#! We say that a walk homomorphism is a two-sided folding if it is sycnhronous and
#! itinduces a bijection beween the set of biinfinite walks of the domain and
#! codomain digraphs.
#!
#! This attribute returns true if and only if the given walk homomorphism is a
#! two-sided folding.
#! <Example> 
#!gap> H := WalkHomomorphism(Digraph([[1, 1]]),Digraph([[1,1]]),[1],[[2],[1]]);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> I := IdentityWalkHomomorphism(Digraph([[1, 1]]));
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> IsTwoSidedFolding(H);         
#!true
#!gap> IsTwoSidedFolding(I);                                                   
#!true
#!gap> H := WalkHomomorphism(Digraph([[1, 1]]),Digraph([[1,1]]),[1],[[2],[2]]);
#!&lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.>
#!gap> IsTwoSidedFolding(H);
#!false
#!gap> D := Digraph([[1, 2, 2], []]);
#!&lt;immutable multidigraph with 2 vertices, 3 edges>
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [3], [2]]);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> IsTwoSidedFolding(W);
#!fail
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [2], [2]]);
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> IsTwoSidedFolding(W);                                
#!fail
#!gap> D := Digraph([[1, 1,  2, 2], []]);                        
#!&lt;immutable multidigraph with 2 vertices, 4 edges>
#!gap> W := WalkHomomorphism(D, D, [1, 2], [[1], [2], [3], [3]]);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 4 edges.>
#!gap> IsTwoSidedFolding(W);                                     
#!true
#!gap> IsOneSidedFolding(W);
#!false
#!</Example>
DeclareAttribute("IsTwoSidedFolding", IsWalkHomomorphism);

#! @Arguments W
#! @Returns a pair of walk homomorphisms
#! @Description
#! It is assummed that the given walk homomorphism is deterministic, the attribute 
#! will return fail if this is not the case.
#!
#! The attribute quotients the domain digraph by the relation that two vertices v, w
#! are equivalent if they map to the same vertex under <A>W</A> and if one reads
#! an edge of the codomain digraph from either of these vertices, then the same
#! vertex is reached. That is to say that if t is the common image of v and w, then
#! for all edges e starting at t, there are edges ev, ew of the domain Which
#! 1. start at v, w respectively. 2. are both mapped to e by <A>W</A>. 3. have 
#! the same target vertex.
#!
#! the first output is the quotient homomorphism q, and the second is the walk 
#! homomorphism w2 such that <A>W</A> is equal to the composite qw2.
#! <Example> 
#!gap> H := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 3, 0);                
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> ReduceSynchronizingLength(H);
#![ &lt;walk homomorphism from a digraph with 16 edges to a digraph with 8 edges.>,
#!  &lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.> ]
#!gap> H := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 3, 1);                
#!&lt;walk homomorphism from a digraph with 32 edges to a digraph with 2 edges.>
#!gap> ReduceSynchronizingLength(H);                                             
#!fail
#!</Example>
DeclareAttribute("ReduceSynchronizingLength", IsWalkHomomorphism);

#! @Arguments W
#! @Returns a list of walk homomorphisms
#! @Description
#! It is assummed that the given walk homomorphism is deterministic, the attribute 
#! will return fail if this is not the case.
#!
#! The attribute reduces the walk homomorphism as in the second output of 
#!  ReduceSynchronizingLength. This is then repeated until the walk homorphism
#! can't be reduced further. The output is the resulting sequence of walk
#! homomorphisms starting with the input and ending with the irreducible
#! one at the end.
#! <Example> 
#!gap> H := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 3, 0);
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> S := SynchronizingSequence(H);
#![ &lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>,
#!  &lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>, 
#!  &lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
#!  &lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.> ]
#!</Example>
DeclareAttribute("SynchronizingSequence", IsWalkHomomorphism);

#! @Arguments W
#! @Returns a list of walk homomorphisms
#! @Description
#! It is assummed that the given walk homomorphism is deterministic, the attribute 
#! will return fail if this is not the case.
#!
#! The attribute reduces the walk homomorphism as in the second output of 
#!  ReduceSynchronizingLength. This is then repeated until the walk homorphism
#! can't be reduced further. The output is the resulting sequence of quotient maps
#! from the first output of ReduceSynchronizingLength but ending with the irreducible
#! output at the end.
#!
#! This is such that the nth entry of SynchronizingSequence(<A>W</A>) is equal
#! to the composite of the nth and later entries of SynchronizingSequenceConnections(<A>W</A>).
#!
#! <Example> 
#!gap> H := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 3, 0);
#!&lt;walk homomorphism from a digraph with 16 edges to a digraph with 2 edges.>
#!gap> SynchronizingSequenceConnections(H);                     
#![ &lt;walk homomorphism from a digraph with 16 edges to a digraph with 8 edges.>,
#!  &lt;walk homomorphism from a digraph with 8 edges to a digraph with 4 edges.>, 
#!  &lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
#!  &lt;walk homomorphism from a digraph with 2 edges to a digraph with 2 edges.> ]
#!</Example>
DeclareAttribute("SynchronizingSequenceConnections", IsWalkHomomorphism);

#! @Arguments D
#! @Returns a walk homomorphisms
#! @Description
#! If the digraph has a pair of vertices which have the same multiset of outneighbours
#! then one can naturally form a quotient of the origional digraph by identifying
#! only these vertices and the corresponding edges eminating from them.
#!
#! This operation reduces the digraph in this fashion as much as possible and
#! returns a homomorphism from the given digraph to a digraph for which no two 
#! vertices has the same multiset of outneighbours. The given walk homomorphism 
#!is always a one-sided folding.
#! <Example> 
#!gap> D := Digraph([[1, 1]]);                                  
#!&lt;immutable multidigraph with 1 vertex, 2 edges>
#!gap> D10 := LineDigraphWalkHomomorphism(D,1,0)!.DomainDigraph;
#!&lt;immutable digraph with 2 vertices, 4 edges>
#!gap> OneSidedDigraphMinimise(D10);      
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> D11 := LineDigraphWalkHomomorphism(D,1,1)!.DomainDigraph;
#!&lt;immutable digraph with 4 vertices, 8 edges>
#!gap> OneSidedDigraphMinimise(D11);                            
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!</Example>
DeclareOperation("OneSidedDigraphMinimise", [IsDigraph]);

#! @Arguments W
#! @Returns a string
#! @Description
#! This operation converts a walk homomorphism into the string the user needs to
#! enter to instruct GAP to generate it.
#! <Example> 
#!gap> WalkHomomorphismInputString(R2toPhiFold());
#!"WalkHomomorphism(Digraph([ [ 1, 1 ] ]), Digraph([ [ 1, 2 ], [ 1 ] ]), [ 1 ],
#! [ [ 1 ], [ 2, 3 ] ])"
#!</Example>
DeclareAttribute("WalkHomomorphismInputString", IsWalkHomomorphism);

#! @Arguments W
#! @Returns a pair of walk homomorphisms
#! @Description
#! By a line folding we mean one of the type constructable by LineDigraphWalkHomomorphism.
#! It is true (see https://arxiv.org/abs/2112.13359) that if <A>W</A> is an 
#! UDAF folding then there is a line folding L and an UDAF folding f such that
#! the composite Lf induces the same UDAF isomorphism as <A>W</A>.
#!
#! If <A>W</A> is an UDAF folding then the operation returns such a pair L, f
#! in that order. Otherwise the operation returns fail.
#! <Example> 
#!gap> FoldingToLineFolding(R2toPhiFold());
#![ &lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
#!  &lt;walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.> ]
#!gap> P := FoldingToLineFolding(R2toPhiFold());
#![ &lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>, 
#!  &lt;walk homomorphism from a digraph with 3 edges to a digraph with 2 edges.> ]
#!gap> H := P[2] * R2toPhiFold();
#!&lt;walk homomorphism from a digraph with 3 edges to a digraph with 3 edges.>
#!gap> RemoveIncompleteResponse(P[1])[1] = RemoveIncompleteResponse(H)[1];
#!true
#!gap> P := FoldingToLineFolding(PhitoR2Fold());
#![ &lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>, 
#!  &lt;walk homomorphism from a digraph with 4 edges to a digraph with 3 edges.> ]
#!gap> H := P[2] * PhitoR2Fold();
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> RemoveIncompleteResponse(P[1]) = RemoveIncompleteResponse(H);
#!true
#!gap> f := WalkHomomorphism(Digraph([[], []]), Digraph([[2], []]), [2, 1], []);
#!&lt;walk homomorphism from a digraph with 0 edges to a digraph with 1 edge.>
#!gap> P := FoldingToLineFolding(f);
#![ &lt;walk homomorphism from a digraph with 0 edges to a digraph with 1 edge.>, 
#!  &lt;walk homomorphism from a digraph with 0 edges to a digraph with 0 edges.> ]
#!gap> H := P[2] * f;
#!&lt;walk homomorphism from a digraph with 0 edges to a digraph with 1 edge.>
#!gap> RemoveIncompleteResponse(P[1]) = RemoveIncompleteResponse(H);
#!true
#!</Example>
DeclareOperation("FoldingToLineFolding", [IsWalkHomomorphism]);