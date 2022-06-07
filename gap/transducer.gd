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
                                                   "MinimalTransducer"]);
                                                   
#! @Arguments T
#! @Returns an isomorphism of subshifts of finite type
#! @Description
#!  Creates an object called a shift homomorphism. A shift isomorphism is a
#!  homomorphisms between subshifts of finite type. This input method requires
#!  a full shift/
#!
#!  This is input as synchronous transducer from the aaa package (TODO ref)
#!  which is strongly synchronizing and such that the map it defines on the 
#!  shift space is a bijection.
#! <Example>gap> 
#!gap> T := Transducer(2, 2, [[1, 2], [1, 2]], [[[1], [1]], [[0], [0]]]);
#!&lt;transducer with input alphabet on 2 symbols, output alphabet on 2 symbols, and 2 states.>
#!gap> ShiftIsomorphism(T);                                              
#!&lt;shift isomorphism whose domain digraph has 2 edges, whose codomain digraph has 2 edges, and which has 2 states.>
#!WalkHomomorphism(Digraph([[1, 1]]), Digraph([[1, 2], [1]]), [1], [[1], [2, 3]]);
#!</Example> 
DeclareOperation("ShiftIsomorphism", [IsTransducer]);
DeclareOperation("ShiftIsomorphism", [IsUDAFTransducer]);
DeclareOperation("ShiftIsomorphism", [IsUDAFTransducer, IsDenseList]);
DeclareOperation("UDAFIsomorphism", [IsUDAFTransducer]);
DeclareOperation("UDAFIsomorphism", [IsTransducer]);
DeclareOperation("UDAFIsomorphism", [IsWalkHomomorphism, IsWalkHomomorphism]);
DeclareOperation("UDAFIsomorphism", [IsShiftIsomorphism]);
DeclareOperation("ComposeShiftIsomorphisms", [IsShiftIsomorphism, 
                                              IsShiftIsomorphism]);
                                              
DeclareOperation("IdentityShiftIsomorphism", [IsPosInt]);
DeclareOperation("DeBruijnTransducer", [IsPosInt, IsPosInt]);
DeclareOperation("UDAFTransducer", [IsTransducer]);
DeclareOperation("UDAFTransducer", [IsWalkHomomorphism, IsWalkHomomorphism]);
DeclareOperation("ComposeUDAFTransducersSlow", [IsUDAFTransducer, 
                                                IsUDAFTransducer,
                                                IsBool]);
DeclareOperation("MinimalUDAFTransducer", [IsUDAFTransducer]);
DeclareAttribute("IsMinimalUDAFTransducer", IsUDAFTransducer);
DeclareSynonymAttr("IsMinimal", IsMinimalUDAFTransducer);
DeclareOperation("AreIsomorphicLabeledUDAFTransducers", [IsUDAFTransducer,
                                                         IsUDAFTransducer,
                                                         IsDenseList,
                                                         IsDenseList]);
DeclareOperation("AreIsomorphicUDAFTransducers", [IsUDAFTransducer,
                                                  IsUDAFTransducer]);
DeclareOperation("\*", [IsShiftIsomorphism, IsShiftIsomorphism]);
DeclareOperation("\*", [IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism]);
DeclareOperation("\*", [IsUDAFTransducer, IsUDAFTransducer]);
DeclareOperation("\*", [IsUDAFIsomorphism, IsUDAFIsomorphism]);
DeclareOperation("\^", [IsUDAFTransducer, IsInt]);
DeclareOperation("\^", [IsShiftIsomorphism, IsInt]);
DeclareOperation("\^", [IsOneSidedShiftIsomorphism, IsInt]);
DeclareOperation("\^", [IsUDAFIsomorphism, IsInt]);
DeclareOperation("\^", [IsUDAFIsomorphism, IsUDAFIsomorphism]);
DeclareOperation("\^", [IsShiftIsomorphism, IsShiftIsomorphism]);
DeclareOperation("\^", [IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism]);
DeclareAttribute("UDAFNrStates", IsUDAFTransducer);
#DeclareOperation("\=", [IsUDAFTransducer, IsUDAFTransducer]);
DeclareOperation("IdentityUDAFTransducer", [IsDigraph]);
DeclareOperation("BlockCodeTransducer", [IsPosInt, IsInt, IsFunction]);
DeclareOperation("SynchronousDomainCombineEquivalentStates", [IsUDAFTransducer]);
DeclareOperation("ResizeZeroStringTransducer", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("IsValidAnnotatedTransducer", [IsUDAFTransducer, IsDenseList]);
DeclareOperation("DeterministicDomainCombineEquivalentStates", [IsUDAFTransducer]);
DeclareOperation("OneSidedShiftIsomorphism", [IsUDAFTransducer]);
DeclareOperation("OneSidedShiftIsomorphism", [IsWalkHomomorphism, 
                                              IsWalkHomomorphism]);
DeclareAttribute("OneSidedTorsionDecomposition", IsOneSidedShiftIsomorphism);
DeclareSynonymAttr("TorsionDecomposition", OneSidedTorsionDecomposition);
DeclareSynonymAttr("TorsionDecomp", OneSidedTorsionDecomposition);
DeclareSynonymAttr("TorDecomp", OneSidedTorsionDecomposition);
DeclareSynonymAttr("FiniteOrderDecomp", OneSidedTorsionDecomposition);
DeclareSynonymAttr("FiniteOrderDecomposition", OneSidedTorsionDecomposition);
DeclareOperation("RandomOneSidedAut", [IsDigraph]);