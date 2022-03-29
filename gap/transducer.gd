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
DeclareOperation("ShiftIsomorphism", [IsTransducer]);
DeclareOperation("ShiftIsomorphism", [IsUDAFTransducer]);
DeclareOperation("ShiftIsomorphism", [IsUDAFTransducer, IsDenseList]);
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
DeclareOperation("AreIsomorphicLabeledUDAFTransducers", [IsUDAFTransducer,
                                                         IsUDAFTransducer,
                                                         IsDenseList,
                                                         IsDenseList]);
DeclareOperation("AreIsomorphicUDAFTransducers", [IsUDAFTransducer,
                                                  IsUDAFTransducer]);
DeclareOperation("\*", [IsShiftIsomorphism, IsShiftIsomorphism]);
DeclareOperation("\*", [IsUDAFTransducer, IsUDAFTransducer]);
DeclareOperation("\^", [IsUDAFTransducer, IsInt]);
DeclareOperation("\*", [IsShiftIsomorphism, IsShiftIsomorphism]);
DeclareOperation("\^", [IsShiftIsomorphism, IsInt]);
DeclareAttribute("UDAFNrStates", IsUDAFTransducer);
#DeclareOperation("\=", [IsUDAFTransducer, IsUDAFTransducer]);
DeclareOperation("IdentityUDAFTransducer", [IsDigraph]);
DeclareOperation("BlockCodeTransducer", [IsPosInt, IsInt, IsFunction]);
DeclareOperation("SynchronousDomainCombineEquivalentStates", [IsUDAFTransducer]);
DeclareOperation("ResizeZeroStringTransducer", [IsPosInt, IsPosInt, IsPosInt]);
DeclareOperation("IsValidAnnotatedTransducer", [IsUDAFTransducer, IsDenseList]);
DeclareOperation("DeterministicDomainCombineEquivalentStates", [IsUDAFTransducer]);
