#############################################################################
##
#W  transducer.gd
#Y  Copyright (C) 2022                               Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declarations of the representation of a transducer and
# that of objects that relate to this package. The appropiate ViewObj functions
# are defined in the transducer.gi file.

#TODO add doc for ! commands
DeclareRepresentation("IsShiftIsomorphism", IsComponentObjectRep and
                      IsAttributeStoringRep, ["Digraph",
                                              "DomainDigraph",
                                              "CoDomainDigraph",
                                              "SynchronousUDAFTransducer",
                                              "MinimalUDAFTransducer",
                                              "Annotation"]);

DeclareRepresentation("IsUDAFTransducer", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["Digraph",
                                              "DomainDigraph",
                                              "CoDomainDigraph",
                                              "DomainFolding",
                                              "CoDomainFolding"]);
DeclareRepresentation("IsUDAFIsomorphism", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["Digraph",
                                              "DomainDigraph",
                                              "CoDomainDigraph",
                                              "DomainFolding",
                                              "CoDomainFolding",
                                              "MinimalUDAFTransducer"]);
DeclareRepresentation("IsOneSidedShiftIsomorphism", IsComponentObjectRep and
                                                    IsAttributeStoringRep,
                                                  ["Digraph",
                                                   "DomainDigraph",
                                                   "CoDomainDigraph",
                                                   "MinimalOneSidedUDAFTransducer"]);

#!@Chapter Transducers and isomorphisms

#!@Section UDAF
#! In this section we descible methods for working with UDAF 
#! transducers and UDAF isomorphisms as described in the paper 
#! (https://arxiv.org/abs/2112.13359). Note that many of the functions here
#! are only implemented for UDAF digraphs for the reasons discussed in the paper.

#TODO add support for two walk homomorphism shift isomorphism input

#! @Arguments T
#! @Returns an isomorphism in the UDAF category
#! @Description
#!  Creates an object called an UDAF isomorphism.
#!
#!  The input is an UDAF transducer, the stored isomorphism is the one obtained
#!  by composing the inverse of the induced domain map with the induced codomain
#!  map.
#! <Example>
#!gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1); 
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0); 
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> T := UDAFTransducer(L01, L10);                               
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 2 states.>
#!gap> UDAFIsomorphism(T);
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!</Example>
DeclareOperation("UDAFIsomorphism", [IsUDAFTransducer]);


#! @Arguments T
#! @Returns an isomorphism in the UDAF category
#! @Description
#!  Creates an object called an UDAF isomorphism (seehttps://arxiv.org/abs/2112.13359).
#!
#!  The input is a core synchronizing aaa transducer object which induces an UDAF 
#!  isomorphism. the stored isomorphism is this induced isomorphism.
#! <Example> 
#!gap> R := ResizeZeroStringGNSTransducer(2, 2, 3);
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 5 states.>
#!gap> U := UDAFIsomorphism(R);
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
#!</Example>
DeclareOperation("UDAFIsomorphism", [IsGNSTransducer]);

#! @Arguments W1, W2
#! @Returns an isomorphism in the UDAF category
#! @Description
#!  Same as UDAFIsomorphism(UDAFTransducer(<A>W1</A>, <A>W2</A>)).
#! <Example>
#!gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1); 
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0); 
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> UDAFIsomorphism(L01, L10);
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!</Example>
DeclareOperation("UDAFIsomorphism", [IsWalkHomomorphism, IsWalkHomomorphism]);

#! @Arguments S
#! @Returns an isomorphism in the UDAF category
#! @Description
#!  Creates an object called an UDAF isomorphism (seehttps://arxiv.org/abs/2112.13359).
#!
#!  The input is a shift isomorphism. Returns the corresponding UDAF isomorphism.
#! <Example> 
#!gap> T := GNSTransducer(2, 2, [[1, 2], [1, 2]], [[[1], [1]], [[0], [0]]]);
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 2 symbols, and 2 states.>
#!gap> ShiftIsomorphism(T);                                              
#!&lt;shift isomorphism whose domain digraph has 2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1], [[1], [2, 3]]);
#!gap> UDAFIsomorphism(S);
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!</Example>
DeclareOperation("UDAFIsomorphism", [IsShiftIsomorphism]);



#! @Arguments T
#! @Returns an UDAF transducer
#! @Description
#! The argument is to be a core synchronizing aaa transducer which induces an
#! UDAF isomorphism.
#!
#! The output is the corresponding UDAF transducer, where the alphabet of
#! size n is replaces with the digraph with one vertex and n edges.
#! <Example> 
#!gap> T := ResizeZeroStringiGNSTransducer(3, 1, 2);;
#!gap> U := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!</Example>
DeclareOperation("UDAFTransducer", [IsGNSTransducer]);

#! @Arguments W1, W2
#! @Returns an UDAF transducer
#! @Description
#! The argument is to be a pair of UDAF foldings with the same domain digraph.
#! The output is the corresponding UDAF transducer.
#! <Example>
#!gap> L11 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 1); 
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!gap> L20 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 2, 0);
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!gap> T := UDAFTransducer(L11, L20);                              
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 4 states.>
#!</Example>
DeclareOperation("UDAFTransducer", [IsWalkHomomorphism, IsWalkHomomorphism]);

#! @Arguments T1, T2
#! @Returns an UDAF transducer
#! @Description
#! The argument is to be a pair of UDAF transducers where the codomain digraph of
#! <A>T1</A> is the same as the domain digraph of <A>T2</A>.
#!
#! The output is an UDAF transducer which induces the composite of the UDAF
#! isomorphisms of the input transducers.
#!
#! This operation can also be called with *.
#! <Example>
#!gap> L11 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 1); 
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!gap> L20 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 2, 0);
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!gap> T := UDAFTransducer(L11, L20);                              
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 4 states.>
#!gap> ComposeUDAFTransducers(T, T);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 8 states.>
#!</Example>
DeclareOperation("ComposeUDAFTransducers", [IsUDAFTransducer, 
                                                IsUDAFTransducer]);
# TODO define the following?    
                                            
#! @Arguments T
#! @Returns an UDAF transducer
#! @Description
#! An UDAF transducer is called minimal if its domain is a one-sided folding,
#! its codomain is an UDAF folding without complete responce, and no two states
#! have both the same domain image and the same codomain image.
#!
#! The operation returns a minimal UDAF transducer which induces the same UDAF
#! isomorphism as the given transducer.
#! <Example>
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> T := UDAFTransducer(L01, L10);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 2 states.>
#!gap> IsMinimalUDAFTransducer(T);
#!false
#!gap> M := MinimalUDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 1 state.>
#!gap> IsMinimalUDAFTransducer(M);            
#!true
#!gap> M = IdentityUDAFTransducer(Digraph([[1, 1]]));
#!true
#!</Example>                                
DeclareOperation("MinimalUDAFTransducer", [IsUDAFTransducer]);

#! @Arguments T
#! @Returns true or false
#! @Description
#! An UDAF transducer is called minimal if its domain is a one-sided folding,
#! its codomain is an UDAF folding with complete responce, and no two states
#! have both the same domain image and the same codomain image. 
#!
#! The attribute returns true if and only if the given transducer is minimal.
#! <Example>
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> L01 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 0, 1);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> T := UDAFTransducer(L01, L10);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 2 states.>
#!gap> IsMinimalUDAFTransducer(T);
#!false
#!gap> M := MinimalUDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 1 state.>
#!gap> IsMinimalUDAFTransducer(M);            
#!true
#!gap> M = IdentityUDAFTransducer(Digraph([[1, 1]]));
#!true
#!</Example>          
DeclareAttribute("IsMinimalUDAFTransducer", IsUDAFTransducer);

DeclareSynonymAttr("IsMinimal", IsMinimalUDAFTransducer);



# (TODO more detail in the following?)

#! @Arguments T1, T2, L1, L2
#! @Returns true or false
#! @Description
#! Two UDAF transducers are called isomorphic if they have the same domain and 
#! codomain digraphs, and there is an isomorphism of their underlying digraphs 
#! which converts one into the other 
#!
#! Here T1, T2 are UDAF transducers and L1, L2 are lists with one entry for each
#! state of T1 and T2 respectievly. These are intended to be thought of as labels
#! for the states of the transdcuers.
#!
#! The attribute returns true if and only if there is an isomorphism from <A>T1</A>
#! to <A>T2</A> which preserves the given labels. 
#! <Example>
#!gap> T := ResizeZeroStringGNSTransducer(3, 1, 2);;
#!gap> U := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> T := ResizeZeroStringGNSTransducer(3, 1, 2);
#!&lt;transducer with input alphabet on 3 symbols, output alphabet on 
#!3 symbols, and 4 states.>
#!gap> U := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> I := U^-1;
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> AreIsomorphicUDAFTransducers(I, U);
#!false
#!gap> M := MinimalUDAFTransducer(I);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> AreIsomorphicUDAFTransducers(M, U);
#!true
#!</Example>         
DeclareOperation("AreIsomorphicLabeledUDAFTransducers", [IsUDAFTransducer,
                                                         IsUDAFTransducer,
                                                         IsDenseList,
                                                         IsDenseList]);
# (TODO more detail in the following?).                              
                                                      
#! @Arguments T1, T2
#! @Returns true or false
#! @Description
#! Two UDAF transducers are called isomorphic if they have the same domain and 
#! codomain digraphs, and there is an isomorphism of their underlying digraphs 
#! which converts one into the other
#!
#! The attribute returns true if and only if the given transducers are isomorphic.
#! <Example>
#!gap> T := ResizeZeroStringGNSTransducer(3, 1, 2);;
#!gap> U := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> T := ResizeZeroStringGNSTransducer(3, 1, 2);
#!&lt;transducer with input alphabet on 3 symbols, output alphabet on 
#!3 symbols, and 4 states.>
#!gap> U := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> I := U^-1;
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> AreIsomorphicUDAFTransducers(I, U);
#!false
#!gap> M := MinimalUDAFTransducer(I);
#!&lt;UDAF Transducer whose domain digraph has 3 edges, whose codomain digraph has 
#!3 edges, and which has 4 states.>
#!gap> AreIsomorphicUDAFTransducers(M, U);
#!true
#!</Example>     
DeclareOperation("AreIsomorphicUDAFTransducers", [IsUDAFTransducer,
                                                  IsUDAFTransducer]);


#! @Arguments T1, T2
#! @Returns an UDAF transducer
#! @Description
#! The argument is to be a pair of UDAF transducers where the codomain digraph of
#! <A>T1</A> is the same as the domain digraph of <A>T2</A>.
#!
#! The output is an UDAF transducer which induces the composite of the UDAF
#! isomorphisms of the input transducers.
#! <Example>
#!gap> L11 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 1); 
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!gap> L20 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 2, 0);
#!&lt;walk homomorphism from a digraph with 8 edges to a digraph with 2 edges.>
#!gap> T := UDAFTransducer(L11, L20);                              
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 4 states.>
#!gap> T * T;
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 8 states.>
#!</Example>
DeclareOperation("\*", [IsUDAFTransducer, IsUDAFTransducer]);

#! @Arguments S1, S2
#! @Returns an UDAF isomorphism
#! @Description
#! Returns the UDAF isomorphism obtained by composing the isomorphisms 
#! <A>S1</A> and <A>S2</A>.
#!
#! This operation can also be called with *.
#! <Example> 
#!gap> T1 := ResizeZeroStringGNSTransducer(2, 2, 3);;
#!gap> U1 := UDAFTransducer(T1);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 5 states.>
#!gap> T2 := ResizeZeroStringGNSTransducer(2, 2, 4);;
#!gap> U2 := UDAFTransducer(T2);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 6 states.>
#!gap> U1 * U2;
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 32 states.>
#!</Example>
DeclareOperation("ComposeUDAFIsomorphisms", [IsUDAFIsomorphism, 
                                              IsUDAFIsomorphism]);

#! @Arguments S1, S2
#! @Returns an UDAF isomorphism
#! @Description
#! Returns the UDAF isomorphism obtained by composing the isomorphisms 
#! <A>S1</A> and <A>S2</A>.
#! <Example> 
#!gap> T1 := ResizeZeroStringGNSTransducer(2, 2, 3);;
#!gap> U1 := UDAFTransducer(T1);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 5 states.>
#!gap> T2 := ResizeZeroStringGNSTransducer(2, 2, 4);;
#!gap> U2 := UDAFTransducer(T2);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 6 states.>
#!gap> U1 * U2;
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 32 states.>
#!</Example>
DeclareOperation("\*", [IsUDAFIsomorphism, IsUDAFIsomorphism]);

#! @Arguments S, n
#! @Returns an UDAF transducer
#! @Description
#! Returns the product of <A>S</A> with itself <A>n</A> times for positive <A>n</A>.
#! Returns the product of the inverse of <A>S</A> with itself |<A>n</A>| times
#! otherwise.
#! <Example> 
#!gap> T1 := ResizeZeroStringGNSTransducer(2, 2, 3);;
#!gap> U1 := UDAFTransducer(T1);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 5 states.>
#!gap> U1^2;
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 16 states.>
#!gap> U1^-1;
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 5 states.>
#!</Example>
DeclareOperation("\^", [IsUDAFTransducer, IsInt]);


#! @Arguments T
#! @Returns an integer
#! @Description
#! Returns the number of states of an UDAF transducer <A>T</A>. That is, the number
#! of vertices of the shared domain of the two UDAF foldings defining <A>T</A>.
#! <Example>
#!gap> T := ResizeZeroStringGNSTransducer(2, 2, 3);;
#!gap> T := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
#!gap> UDAFNrStates(T);
#!5
#!</Example>
DeclareAttribute("UDAFNrStates", IsUDAFTransducer);
#DeclareOperation("\=", [IsUDAFTransducer, IsUDAFTransducer]);

#! @Arguments D
#! @Returns an UDAF transducer
#! @Description
#! Returns the UDAF transducer defined using two copies of the identity folding
#! on the given digraph.
#! <Example>
#!gap> T := IdentityUDAFTransducer(PetersenGraph());
#!&lt;UDAF Transducer whose domain digraph has 
#!30 edges, whose codomain digraph has 30 edges, and which has 10 states.>
#!</Example>
DeclareOperation("IdentityUDAFTransducer", [IsDigraph]);


#! @Arguments T
#! @Returns an UDAF transducer and a list of lists of integers
#! @Description
#! It is assumed that the domain folding of <A>T</A> is deterministic.
#! The operation returns the transducer obtained from <A>T</A> by quotienting the
#! the underlying digraph of <A>T</A> as much as possible such that the UDAF
#! foldings of <A>T</A> are still well defined.
#!
#! The operation also returns the list of equivalence classes of vertices of the
#! above relation.
#! <Example>
#!gap> L10 := LineDigraphWalkHomomorphism(Digraph([[1, 1]]), 1, 0);
#!&lt;walk homomorphism from a digraph with 4 edges to a digraph with 2 edges.>
#!gap> U := UDAFTransducer(L10, L10);
#!&lt;UDAF Transducer whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> DeterministicDomainCombineEquivalentStates(U);
#![ &lt;UDAF Transducer whose domain digraph has 
#!    2 edges, whose codomain digraph has 2 edges, and which has 1 state.>, 
#!  [ [ 1, 2 ] ] ]
#!</Example>
DeclareOperation("DeterministicDomainCombineEquivalentStates", [IsUDAFTransducer]);


#! @Arguments S, n
#! @Returns an UDAF isomorphism
#! @Description
#! Returns the product of <A>S</A> with itself <A>n</A> times for positive <A>n</A>.
#! Returns the product of the inverse of <A>S</A> with itself |<A>n</A>| times
#! otherwise.
#! <Example> 
#!gap> T1 := ResizeZeroStringGNSTransducer(2, 2, 3);;      
#1gap> U1 := UDAFIsomorphism(T1); 
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
#!gap> U1^2;                     
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!gap> U1^-1;
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
#!</Example>
DeclareOperation("\^", [IsUDAFIsomorphism, IsInt]);

#! @Arguments S, T
#! @Returns an UDAF isomorphism
#! @Description
#! Returns the conjugate of <A>S</A> by <A>T</A>. That is to say the product <A>T</A>^-1 <A>S</A> <A>T</A>.
#! <Example> 
#!gap> T1 := ResizeZeroStringGNSTransducer(2, 2, 3);;      
#!gap> U1 := UDAFIsomorphism(T1); 
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
#!gap> U1^2;                     
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!gap> U1^-1;
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 5 states.>
#!gap> T2 := ResizeZeroStringGNSTransducer(2, 2, 4);;
#!gap> U2 := UDAFIsomorphism(T2);
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 6 states.>
#!gap> U1^U2;
#!&lt;UDAF Isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 6 states.>
#!</Example>
DeclareOperation("\^", [IsUDAFIsomorphism, IsUDAFIsomorphism]);


















#!@Section Two-sided shift
#! In this section we describe methods for working with isomorphisms
#! between subshifts of finite type. If D is a finite digraph, then the 
#! corresponding shift space is defined to be the topological space of biinfinite
#! edge walks throught D with the product topology. We call such a space a shift
#! space. We call the dynamical system consisting of this space and the homomorphism
#! defined by shifting the indexing of a walk by one, a subshift of finite type.
#! If f is any isomorphism between the subshifts of finite type corresponding to 
#! two digraphs A and B, then one can find a third digraph C and a pair of two-sided
#! f1:C->A and f2:C->B such that f is equal to the composit of the inverse of the
#! homeomorphism induced by f1 with the homeomorphism induced by f2. 
#! This is how shift isomorphisms are handled in this package.
#! Many of the functions in this section use UDAF transducers as a base and hense
#! require the given digraphs to be UDAF digraphs.

#! @Arguments T
#! @Returns an isomorphism of subshifts of finite type
#! @Description
#!  Creates an object called a shift isomorphism. A shift isomorphism is an
#!  isomorphism between subshifts of finite type. This input method requires
#!  a full shift.
#!
#!  This is input as synchronous transducer from the aaa package (https://github.com/gap-packages/aaa)
#!  which is strongly synchronizing and such that the map it defines on the 
#!  shift space is a bijection.
#! <Example>gap> 
#!gap> T := GNSTransducer(2, 2, [[1, 2], [1, 2]], [[[1], [1]], [[0], [0]]]);
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 2 symbols, and 2 states.>
#!gap> ShiftIsomorphism(T);                                              
#!&lt;shift isomorphism whose domain digraph has 2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1], [[1], [2, 3]]);
#!</Example>
DeclareOperation("ShiftIsomorphism", [IsGNSTransducer]);

#! @Arguments T
#! @Returns an isomorphism of subshifts of finite type
#! @Description
#!  Creates an object called a shift homomorphism. A shift isomorphism is a
#!  homomorphism between subshifts of finite type. 
#!
#!  This is input as an UDAF transducer for which both UDAF foldings are two-sided
#!  foldings. The isomorphism is the composite of the inverse of the
#!  homeomorphism induced by the first folding with the homeomorphism induced by
#!  the second folding.
#! <Example> 
#!gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1, 1, 1]])));
#!&lt;shift isomorphism whose domain digraph has 
#!4 edges, whose codomain digraph has 4 edges, and which has 1 state.>
#!gap> T := BlockCodeGNSTransducer(2, 2, x-> [x[1]]);              
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 4 states.>
#!gap> T := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 4 states.>
#!gap> S := ShiftIsomorphism(T);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 4 states.>
#!</Example>
DeclareOperation("ShiftIsomorphism", [IsUDAFTransducer]);


#(TODO add reference to explanation of this).

#! @Arguments T
#! @Returns an isomorphism of subshifts of finite type
#! @Description
#!  Creates an object called a shift isomorphism. A shift isomorphism is an
#!  isomorphism between subshifts of finite type. 
#!
#!  This is input as a minimal UDAF transducer and a valid annotation of its 
#!  codomain folding. The isomorphism is the composite of the inverse of the
#!  homeomorphism induced by the first folding with the homeomorphism induced by
#!  the second folding using the annotation.
#! <Example> 
#!gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])),
#!> [-1]);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> T := BlockCodeGNSTransducer(2, 2, x-> [x[1]]);
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 4 states.>
#!gap> T := UDAFTransducer(T);
#!&lt;UDAF Transducer whose domain digraph has 2 edges, whose codomain digraph has 
#!2 edges, and which has 4 states.>
#!gap> S := ShiftIsomorphism(T);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 4 states.>
#!gap> S2 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [2]);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 4 states.>
#!gap> S=S2;
#!true
#!</Example>
DeclareOperation("ShiftIsomorphism", [IsUDAFTransducer, IsDenseList]);


#! @Arguments S1, S2
#! @Returns a shift isomorphism
#! @Description
#! Returns the shift isomorphism obtained by composing the homeomorphisms 
#! <A>S1</A> and <A>S2</A>.
#!
#! This operation can also be called with *.
#! <Example> 
#!gap> SM1 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [-1]);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> S0 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [0]); 
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!gap> S1 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [1]);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> SM1 * S1 = S0;
#!true
#!</Example>
DeclareOperation("ComposeShiftIsomorphisms", [IsShiftIsomorphism, 
                                              IsShiftIsomorphism]);    

#! @Arguments S1, S2
#! @Returns a shift isomorphism
#! @Description
#! Returns the shift isomorphism obtained by composing the homeomorphisms 
#! <A>S1</A> and <A>S2</A>.
#! <Example> 
#!gap> SM1 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [-1]);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> S0 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [0]); 
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!gap> S1 := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])), [1]);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> SM1 * S1 = S0;
#!true
#!</Example>
DeclareOperation("\*", [IsShiftIsomorphism, IsShiftIsomorphism]);

#! @Arguments S, T
#! @Returns a shift isomorphism
#! @Description
#! Returns the conjugate of <A>S</A> by <A>T</A>. That is to say the product <A>T</A>^-1 <A>S</A> <A>T</A>.
#! <Example>
#!gap> C := GNSTransducer(3, 3, [[1, 1, 1]], [[[1], [2], [0]]]);               
#!&lt;transducer with input alphabet on 3 symbols, output alphabet on 
#!3 symbols, and 1 state.>
#!gap> C:= ShiftIsomorphism(UDAFTransducer(C));        
#!&lt;shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 1 state.>
#!gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5 := ShiftIsomorphism(UDAFTransducer(Fig5L, Fig5R));
#!&lt;shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 3 states.>
#!gap> Fig5^C;
#!&lt;shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 9 states.>
#!</Example>
DeclareOperation("\^", [IsShiftIsomorphism, IsShiftIsomorphism]);

#! @Arguments n
#! @Returns a shift isomorphism
#! @Description
#! Returns the identity shift isomorphism on for the shift space of the digraph
#! with one vertex and <A>n</A> edges (for <A>n</A> at least 2).
#! <Example> 
#!gap> IdentityShiftIsomorphism(2);
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 1 state.>
#!gap> IdentityShiftIsomorphism(4);
#!&lt;shift isomorphism whose domain digraph has 
#!4 edges, whose codomain digraph has 4 edges, and which has 1 state.>
#!</Example>
DeclareOperation("IdentityShiftIsomorphism", [IsPosInt]);


#! @Arguments S, n
#! @Returns a shift isomorphism
#! @Description
#! Returns the product of <A>S</A> with itself <A>n</A> times for positive <A>n</A>.
#! Returns the product of the inverse of <A>S</A> with itself |<A>n</A>| times
#! otherwise.
#! <Example> 
#!gap> S := ShiftIsomorphism(IdentityUDAFTransducer(Digraph([[1, 1]])),
#!> [1]); 
#!&lt;shift isomorphism whose domain digraph has 
#!2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!gap> S!.Annotation;
#![ 1 ]
#!gap> (S^3)!.Annotation;
#![ 3 ]
#!gap> (S^-2)!.Annotation;
#![ -2 ]
#!</Example>
DeclareOperation("\^", [IsShiftIsomorphism, IsInt]);


































#!@Section One-sided shift
#! In this section we describe methods for working with isomorphisms
#! between one-sided subshifts of finite type. If D is a finite digraph, then the 
#! corresponding one-sided shift space is defined to be the topological space of 
#! backwards infinite edge walks through D with the product topology. We call 
#! such a space a one-sided shift space. We call the dynamical system consisting
#! of this space and the continuous map defined by deleting the last edge in a walk
#! (and reindexing the remaining edges) a one-sided subshift of finite type.
#! If f is any isomorphism between the one-sided subshifts of finite type corresponding to 
#! two digraphs A and B, then one can find a third digraph C and a pair of one-sided foldings
#! f1:C->A and f2:C->B such that f is equal to the composit of the inverse of the
#! homeomorphism induced by f1 with the homeomorphism induced by f2. 
#! This is how one-sided shift isomorphisms are handled in this package.
#! Many of the functions in this section use UDAF transducers as a base and hense
#! require the given digraphs to be UDAF digraphs.

#! @Arguments T
#! @Returns an isomorphism of one-sided subshifts of finite type
#! @Description
#!  Creates an object called a one-sided shift homomorphism. A one-sided shift 
#!  isomorphism is a homomorphism between the backwards infinite walk spaces of
#!  domain and codomain digraph which commutes with the shift map (which removes
#!  the last edge in the walk).
#!
#!  This is input as an UDAF transducer for which both UDAF foldings are one-sided
#!  foldings. The isomorphism is the composite of the inverse of the
#!  homeomorphism induced by the first folding with the homeomorphism induced by
#!  the second folding.
#! <Example> 
#!gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5 := UDAFTransducer(Fig5L, Fig5R); 
#!gap> Fig5 := OneSidedShiftIsomorphism(Fig5); 
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
#!</Example>
DeclareOperation("OneSidedShiftIsomorphism", [IsUDAFTransducer]);

#! @Arguments F1, F2
#! @Returns an isomorphism of one-sided subshifts of finite type
#! @Description
#!  Creates an object called a one-sided shift homomorphism. A one-sided shift 
#!  isomorphism is a homomorphism between the backwards infinite walk spaces of
#!  domain and codomain digraph which commutes with the shift map (which removes
#!  the last edge in the walk).
#!
#!  This is input as a pair of one-sided foldings. The isomorphism is the 
#!  composite of the inverse of the homeomorphism induced by the first folding 
#!  with the homeomorphism induced by the second folding.
#! <Example> 
#!gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5 := OneSidedShiftIsomorphism(Fig5L, Fig5R); 
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
#!</Example>
DeclareOperation("OneSidedShiftIsomorphism", [IsWalkHomomorphism, 
                                              IsWalkHomomorphism]);

#! @Arguments S1, S2
#! @Returns a one-sided shift isomorphism
#! @Description
#! Returns the one-sided shift isomorphism obtained by composing the homeomorphisms 
#! <A>S1</A> and <A>S2</A>.
#!
#! This operation can also be called with *.
#! <Example> 
#!gap> f := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
#!> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
#!> [ [ 3 ], [ 4 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 1 ],
#!> [ 2 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
#!gap> g := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
#!> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
#!> [ [ 3 ], [ 4 ], [ 4 ], [ 3 ], [ 2 ], [ 1 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], 
#!> [ 1 ], [ 2 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
#!gap> T := OneSidedShiftIsomorphism(f, g);
#!&lt;one sided shift isomorphism whose domain digraph has 
#!4 edges, whose codomain digraph has 4 edges, and which has 6 states.>
#!gap> S := OneSidedTorsionDecomposition(T);
#![ &lt;one sided shift isomorphism whose domain digraph has 
#!    4 edges, whose codomain digraph has 4 edges, and which has 3 states.>, 
#!  &lt;one sided shift isomorphism whose domain digraph has 
#!    4 edges, whose codomain digraph has 4 edges, and which has 5 states.> ]
#!gap> S[1] * S[2] = T;
#!true
#!</Example>
DeclareOperation("ComposeOneSidedShiftIsomorphisms", [IsOneSidedShiftIsomorphism, 
                                              IsOneSidedShiftIsomorphism]);
                                              
#! @Arguments S1, S2
#! @Returns a one-sided shift isomorphism
#! @Description
#! Returns the one-sided shift isomorphism obtained by composing the homeomorphisms 
#! <A>S1</A> and <A>S2</A>.
#! <Example> 
#!gap> f := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
#!> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
#!> [ [ 3 ], [ 4 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 1 ],
#!> [ 2 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
#!gap> g := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
#!> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
#!> [ [ 3 ], [ 4 ], [ 4 ], [ 3 ], [ 2 ], [ 1 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], 
#!> [ 1 ], [ 2 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
#!gap> T := OneSidedShiftIsomorphism(f, g);
#!&lt;one sided shift isomorphism whose domain digraph has 
#!4 edges, whose codomain digraph has 4 edges, and which has 6 states.>
#!gap> S := OneSidedTorsionDecomposition(T);
#![ &lt;one sided shift isomorphism whose domain digraph has 
#!    4 edges, whose codomain digraph has 4 edges, and which has 3 states.>, 
#!  &lt;one sided shift isomorphism whose domain digraph has 
#!    4 edges, whose codomain digraph has 4 edges, and which has 5 states.> ]
#!gap> S[1] * S[2] = T;
#!true
#!</Example>
DeclareOperation("\*", [IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism]);

#! @Arguments S, T
#! @Returns a one-sided shift isomorphism
#! @Description
#! Returns the conjugate of <A>S</A> by <A>T</A>. That is to say the product <A>T</A>^-1 <A>S</A> <A>T</A>.
#! <Example>
#!gap> C := GNSTransducer(3, 3, [[1, 1, 1]], [[[1], [2], [0]]]);
#!&lt;transducer with input alphabet on 3 symbols, output alphabet on 
#!3 symbols, and 1 state.>
#!gap> C:= OneSidedShiftIsomorphism(UDAFTransducer(C));
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 1 state.>
#!gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5 := OneSidedShiftIsomorphism(Fig5L, Fig5R);  
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
#!gap> Fig5^C;
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
#!</Example>
DeclareOperation("\^", [IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism]);

#! @Arguments S, n
#! @Returns a one-sided shift isomorphism
#! @Description
#! Returns the product of <A>S</A> with itself <A>n</A> times for positive <A>n</A>.
#! Returns the product of the inverse of <A>S</A> with itself |<A>n</A>| times
#! otherwise.
#! <Example> 
#!gap> Fig5L := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[1], [2], [3], [1], [2], [3], [1], [2], [3]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5R := WalkHomomorphism(Digraph([[1, 2, 3], [1, 2, 3], [1, 2, 3]]),
#!> Digraph([[1, 1, 1]]),
#!> [1, 1, 1],
#!> [[3], [1], [2], [3], [2], [1], [3], [2], [1]]);
#!&lt;walk homomorphism from a digraph with 9 edges to a digraph with 3 edges.>
#!gap> Fig5 := OneSidedShiftIsomorphism(Fig5L, Fig5R);      
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
#!gap> Fig5^2;
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 3 states.>
#!gap> Fig5^-1;
#!&lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 2 states.>
#!</Example>
DeclareOperation("\^", [IsOneSidedShiftIsomorphism, IsInt]);



#! @Arguments T
#! @Returns a list of one-sided shift isomorphisms
#! @Description
#! It is required that the given isomorphism have the same domain and codomain digraph.
#! Returns a list of one-sided shift isomorphisms of finite order whose composite
#! is the given isomorphism <A>T</A>. This is done via an slight generalisation
#! of the algorithm presented in https://arxiv.org/abs/2004.08478v4by.
#! <Example> 
#!gap> f := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
#!> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
#!> [ [ 3 ], [ 4 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], [ 1 ],
#!> [ 2 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
#!gap> g := WalkHomomorphism(Digraph([ [ 6, 3 ], [ 5, 3 ], [ 2, 1 ], [ 5, 3 ], 
#!> [ 1, 2 ], [ 1, 4 ] ]), Digraph([ [ 2, 2 ], [ 1, 1 ] ]), [ 2, 2, 1, 2, 1, 1 ], 
#!> [ [ 3 ], [ 4 ], [ 4 ], [ 3 ], [ 2 ], [ 1 ], [ 3 ], [ 4 ], [ 1 ], [ 2 ], 
#!> [ 1 ], [ 2 ] ]);
#!&lt;walk homomorphism from a digraph with 12 edges to a digraph with 4 edges.>
#!gap> T := OneSidedShiftIsomorphism(f, g);
#!&lt;one sided shift isomorphism whose domain digraph has 
#!4 edges, whose codomain digraph has 4 edges, and which has 6 states.>
#!gap> S := OneSidedTorsionDecomposition(T);
#![ &lt;one sided shift isomorphism whose domain digraph has 
#!    4 edges, whose codomain digraph has 4 edges, and which has 3 states.>, 
#!  &lt;one sided shift isomorphism whose domain digraph has 
#!    4 edges, whose codomain digraph has 4 edges, and which has 5 states.> ]
#!gap> S[1] * S[2] = T;
#!true
#!</Example>
DeclareAttribute("OneSidedTorsionDecomposition", IsOneSidedShiftIsomorphism);

DeclareSynonymAttr("TorsionDecomposition", OneSidedTorsionDecomposition);
DeclareSynonymAttr("TorsionDecomp", OneSidedTorsionDecomposition);
DeclareSynonymAttr("TorDecomp", OneSidedTorsionDecomposition);
DeclareSynonymAttr("FiniteOrderDecomp", OneSidedTorsionDecomposition);
DeclareSynonymAttr("FiniteOrderDecomposition", OneSidedTorsionDecomposition);


#! @Arguments D
#! @Returns a one-sided shift isomorphism
#! @Description
#! Returns a random one-sided isomorphism with <A>D</A> as it's domain and codomain
#!  digraph.
#!
#! Warning this commant is currently very slow even on rather small examples.
#! <Example> 
#!gap> RandomOneSidedAut(Digraph([[1, 1, 1]]));
#! &lt;one sided shift isomorphism whose domain digraph has 
#!3 edges, whose codomain digraph has 3 edges, and which has 3 states.>
#!</Example>
DeclareOperation("RandomOneSidedAut", [IsDigraph]);






















#!@Section GNS transducers (from aaa).
#! By a GNS transducer we mean a transducer of the type
#! described by Grigorchuk, Nekrashevich, and Sushchanskii as defined in the 
#!  package aaa (https://github.com/gap-packages/aaa).

#! @Arguments AlphSize, History, BlockMap
#! @Returns an aaa transducer object
#! @Description
#!  <A>AlphSize</A> is assumed to be an integer which is at least 2. <A>History</A> is assumed
#!  to be a non-negative integer. <A>BlockMap</A> is a function which assigns each word
#!  over the alphabet [0, 1, ..., <A>AlphSize</A> - 1] of length <A>History</A> + 1
#!  another word over the same alphbet.
#!
#!  The output object is an aaa (https://github.com/gap-packages/aaa) transducer
#!  whose input and output alphabets have <A>AlphSize</A> letters. There is a
#!  state for each word of length <A>History</A> in the alphbet. Transitions are
#!  done as is DeBruijin graphs, that is a letter a is read from a state w by 
#!  transitioning to the state which is a suffix of the word wa. In this case
#!  the word wrtten that the word objetain by applying the function <A>BlockMap</A>
#!  to wa.
#!
#! <Example> 
#!gap> T := BlockCodeGNSTransducer(2, 2, x-> [x[1]]);              
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 4 states.>
#!</Example>
DeclareOperation("BlockCodeGNSTransducer", [IsPosInt, IsInt, IsFunction]);

#! @Arguments AlphSize, History
#! @Returns an aaa transducer
#! @Description
#!  The output object is an aaa (https://github.com/gap-packages/aaa) transducer
#!  whose input and output alphabets have <A>AlphSize</A> letters. There is a
#!  state for each word of length <A>History</A> in the alphbet. Transitions are
#!  done as is DeBruijin graphs, that is a letter a is read from a state w by 
#!  transitioning to the state which is a suffix of the word wa.
#!  The write function is the identity function.
#! <Example> 
#!gap> DeBruijnGNSTransducer(2, 3);                       
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 8 states.>
#!</Example>
DeclareOperation("DeBruijnGNSTransducer", [IsPosInt, IsPosInt]);


#! @Arguments AlphSize, Len1, Len2
#! @Returns an aaa transducer
#! @Description
#! It is required that <A>AlphSize</A> is at least 2. Returns the minimal aaa 
#! transducer which defines the homeomorphism of cantor space defined by
#! finding all maximal contiguous all zero substrings in the
#! given input, and replacing those with length <A>Len1</A> with those with
#! length <A>Len2</A> and vice versa.
#! <Example>
#!gap> ResizeZeroStringGNSTransducer(2, 1, 3);
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 5 states.>
#!gap> ResizeZeroStringGNSTransducer(3, 2, 1);
#!&lt;transducer with input alphabet on 3 symbols, output alphabet on 
#!3 symbols, and 4 states.>
#!</Example>
DeclareOperation("ResizeZeroStringGNSTransducer", [IsPosInt, IsPosInt, IsPosInt]);
                          


#! @Arguments T
#! @Returns true or false
#! @Description
#!  We say that a GNS trasducer T is Lipschitz if for each state of T defines a 
#!  well-defined function from the set of forwards infinite words in the domain 
#!  alphabet to the set of infinite words, and these maps are all Lipschitz 
#!  continuous.
#!
#!  This is done with respect to the metric:
#!
#!   d(x_0x_1x_2..., y_0y_1y_2...) := inf({1/2^n|where n is such that x_i=y_i for all i &lt; n}).
#!
#!  Note that it is possible for the minimal transducer of a Lipschitz function
#!  to not be Lipschitz as the minimal transducer of a map with a constant state
#!  will have states which do not give output.
#! <Example>
#!gap> T := GNSTransducer(2, 2, [[2, 2], [1, 1]],[[[],[]],[[1],[1]]]);;
#!gap> IsLipschitzGNSTransducer(T);
#!true
#!gap> T := AlphabetChangeGNSTransducer(4, 3);;
#!gap> IsLipschitzGNSTransducer(T);
#!true
#!gap> T := GNSTransducer(3, 3, [[2, 2, 1], [3, 4, 3], [1, 1, 2], [2, 2, 3]],
#!> [[[1, 0, 1], [0], [2, 2, 0]], [[], [2], [0, 1, 2, 2]], [[], [0, 1], [2, 2]],
#!> [[2, 2], [0, 2], [1, 2, 2]]]);;
#!gap> IsLipschitzGNSTransducer(T);
#!false
#!gap> T := AlphabetChangeGNSTransducer(3, 4);;
#!gap> IsLipschitzGNSTransducer(T);
#!false
#!gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> IsLipschitzGNSTransducer(T);
#!false
#!gap> M := MinimalGNSTransducer(T);                                      
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 2 states.>
#!gap> IsLipschitzGNSTransducer(T);                                          
#!true
#!gap> IsLipschitzGNSTransducer(M);                                          
#!false
#!</Example>
DeclareOperation("IsLipschitzGNSTransducer", [IsGNSTransducer]);

#! @Arguments T
#! @Returns an aaa transducer
#! @Description
#!  If T is a synchronizing transducer, then we define the core of T to be the
#!  smallest non-empty transducer obtainable from T by removing states from T.
#!
#!  The Operation takes as input an synchronizing transducer and returns its Core
#! <Example>
#!gap> T := GNSTransducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
#!> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
#!gap> C := GNSTransducerCore(T);;
#!gap> OutputFunction(C);
#![ [ [ 1 ], [ 0, 1 ] ], [ [ 1 ], [  ] ], [ [ 1 ], [ 0 ] ] ]
#!gap> TransitionFunction(C);
#![ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
#!gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> C := GNSTransducerCore(T);;
#!gap> OutputFunction(C);
#![ [ [ 1, 0 ], [  ] ], [ [ 0 ], [ 1, 1 ] ], [ [ 0 ], [ 1 ] ] ]
#!gap> TransitionFunction(C);
#![ [ 1, 2 ], [ 1, 3 ], [ 1, 3 ] ]
#!gap> T := GNSTransducer(2, 2, [[2, 2], [1, 1]], [[[1], [1]], [[0], [0]]]);
#!transducer with input alphabet on 2 symbols, output alphabet on 
#!2 symbols, and 2 states.>
#!gap> GNSTransducerCore(T);
#!Error, autshift: GNSTransducerCore: usage,
#!the transducer must be synchronizing 
#!</Example>
DeclareOperation("GNSTransducerCore", [IsGNSTransducer]);

#! @Arguments T
#! @Returns true or false
#! @Description
#!  If T is a synchronizing transducer, then we define the core of T to be the
#!  smallest non-empty transducer obtainable from T by removing states from T.
#!  We say that T is core if it is equal to its core.
#!
#!  The attribute returns true if and only if the given transducer is core.
#! <Example>
#!gap> T := GNSTransducer(2, 2, [[2, 3], [3, 4], [3, 2], [3, 4]],
#!> [[[1], [1, 0, 1]], [[1], []], [[1], [0, 1]], [[1], [0]]]);;
#!gap> IsCoreGNSTransducer(T);
#!false
#!gap> C := GNSTransducerCore(T);;
#!gap> IsCoreGNSTransducer(C);
#!true
#!gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> IsCoreGNSTransducer(T);
#!true
#!gap> T := GNSTransducer(2, 2, [[1, 2], [1, 3], [1, 3]], [[[1, 0], []], [[0],
#!> [1, 1]], [[0], [1]]]);;
#!gap> IsCoreGNSTransducer(T);
#!true
#!gap> T := GNSTransducer(2, 4, [[1, 2], [1, 3], [1, 1]], [[[0], []], [[1], []],
#!> [[2], [3]]]);;
#!gap> IsCoreGNSTransducer(T);
#!false
#!gap> T := DeBruijnGNSTransducer(2, 3);;
#!gap> IsCoreGNSTransducer(T);
#!true
#!gap> T := DeBruijnGNSTransducer(3, 2);;
#!gap> IsCoreGNSTransducer(T);
#!true
#!</Example>
DeclareAttribute("IsCoreGNSTransducer", IsGNSTransducer);
