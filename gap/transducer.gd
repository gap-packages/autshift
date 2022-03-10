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

DeclareRepresentation("IsShiftMorphism", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["InputAlphabet",
                                              "OutputAlphabet",
                                              "NrStates",
                                              "TransitionFunction",
                                              "OutputFunction",
                                              "TransducerFunction"]);
DeclareOperation("ShiftMorphism", [IsTransducer]);
DeclareOperation("IdentityShiftMorphism", [IsPosInt]);
DeclareOperation("DeBruijnTransducer", [IsPosInt, IsPosInt]);

DeclareRepresentation("IsUDAFIsomorphism", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["Digraph",
                                              "DomainDigraph",
                                              "CoDomainDigraph",
                                              "DomainFolding",
                                              "CoDomainFolding"]);

DeclareOperation("UDAFIsomorphism", [IsTransducer]);
DeclareOperation("UDAFIsomorphism", [IsWalkHomomorphism, IsWalkHomomorphism]);
DeclareOperation("ComposeUDAFIsomorphisms", [IsUDAFIsomorphism, IsUDAFIsomorphism]);
