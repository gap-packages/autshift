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

DeclareRepresentation("IsWalkHomomorphism", IsComponentObjectRep and
                      IsAttributeStoringRep,
                                             ["DomainDigraph",
                                              "CoDomainDigraph",
                                              "VertexMap",
                                              "EdgeMap"]);
DeclareOperation("WalkHomomorphism", [IsDigraph, IsDigraph, IsDenseList, IsDenseList]);
DeclareAttribute("IsDegenerateWalkHomomorphism", IsWalkHomomorphism);
DeclareOperation("SynchronousWalkHomomorphism", [IsWalkHomomorphism]);
DeclareOperation("R2toPhiFold", []);
DeclareOperation("PhitoR2Fold", []);
DeclareOperation("WalkHomomorphismVertexImageAutomaton", [IsWalkHomomorphism, IsInt]);
DeclareAttribute("WalkHomomorphismImageAutomaton", IsWalkHomomorphism);
DeclareOperation("PowerSetWalkHomomorphism", [IsWalkHomomorphism]);
DeclareAttribute("ImageFinderWalkHomomorphism", IsWalkHomomorphism);
DeclareOperation("DualWalkHomomorphism", [IsWalkHomomorphism]);
DeclareOperation("WalksOfGivenLength", [IsDigraph, IsInt]);
DeclareAttribute("OutEdgesAtVertex", IsDigraph);
DeclareOperation("IdentityWalkHomomorphism", [IsDigraph]);
DeclareOperation("ImageAsUnionOfCones", [IsWalkHomomorphism, IsInt]);
DeclareAttribute("ImagesAsUnionsOfCones", IsWalkHomomorphism);
DeclareOperation("ComposeWalkHomomorphisms", [IsWalkHomomorphism, IsWalkHomomorphism]);
DeclareOperation("LineDigraphWalkHomomorphism", [IsDigraph, IsInt, IsInt]);
DeclareOperation("FoldingToLineFolding", [IsWalkHomomorphism]);
DeclareOperation("TrimWalkHomomorphism", [IsWalkHomomorphism]);
DeclareAttribute("IsUDAFDigraph", IsDigraph);
DeclareAttribute("IsUDAFFolding", IsWalkHomomorphism);
DeclareAttribute("MaxFutureConeDepth", IsWalkHomomorphism);
DeclareAttribute("MaxHistoryConeDepth", IsWalkHomomorphism);
DeclareAttribute("IsDeterministicWalkHomomorphism", IsWalkHomomorphism);
DeclareOperation("\*", [IsWalkHomomorphism, IsWalkHomomorphism]);
#DeclareOperation("\=", [IsWalkHomomorphism, IsWalkHomomorphism]);
DeclareOperation("RemoveIncompleteResponse", [IsWalkHomomorphism]);
DeclareAttribute("IsSynchronousWalkHomomorphism", IsWalkHomomorphism);
DeclareOperation("SynchronousRemoveIncompleteResponse", [IsWalkHomomorphism]);
DeclareAttribute("IsAnnotatableWalkHomomorphism", IsWalkHomomorphism);
DeclareOperation("WalkHomomorphismAnnotation", [IsWalkHomomorphism, IsInt, IsInt]);
DeclareOperation("WalkHomomorphismAnnotation", [IsWalkHomomorphism, IsInt]);
DeclareOperation("WalkHomomorphismAnnotation", [IsWalkHomomorphism]);
DeclareAttribute("IsOneSidedFolding", IsWalkHomomorphism);
DeclareAttribute("IsTwoSidedFolding", IsWalkHomomorphism);
DeclareAttribute("ReduceSynchronizingLength", IsWalkHomomorphism);
DeclareAttribute("SynchronizingSequenceConnections", IsWalkHomomorphism);
DeclareAttribute("SynchronizingSequence", IsWalkHomomorphism);
DeclareOperation("OneSidedDigraphMinimise", [IsDigraph]);
DeclareAttribute("WalkHomomorphismInputString", IsWalkHomomorphism);