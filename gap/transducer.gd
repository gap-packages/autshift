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
DeclareOperation("\*", [IsShiftIsomorphism, IsShiftIsomorphism]);
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