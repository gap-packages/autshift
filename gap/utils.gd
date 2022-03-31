#############################################################################
##
#W  utils.gd
#Y  Copyright (C) 2022                               Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains the declaration of the utility functions.

DeclareGlobalFunction("AutshiftMakeDoc");
DeclareOperation("DotWalkHomomorphism", [IsWalkHomomorphism]);
DeclareOperation("DotUDAFTransducer", [IsUDAFTransducer]);
DeclareOperation("DotUDAFTransducerWithVertexLabels", [IsUDAFTransducer, IsDenseList]);
DeclareOperation("DotShiftIsomorphism", [IsShiftIsomorphism]);
DeclareOperation("DotShiftIsomorphismAnnotatedUDAF", [IsShiftIsomorphism]);
DeclareOperation("Draw", [IsWalkHomomorphism]);
DeclareOperation("Draw", [IsShiftIsomorphism]);
DeclareOperation("DrawSynchronous", [IsShiftIsomorphism]);
DeclareOperation("Draw", [IsUDAFTransducer]);
DeclareOperation("Draw", [IsUDAFIsomorphism]);
