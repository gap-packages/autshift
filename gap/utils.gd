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

#! @Arguments T
#! @Returns a string
#! @Description
#!  Given a walk homomorphism, this operation returns a dot string for creating
#!  an image of the walk homomorphism. The vertices of the domain digraph are 
#!  marked by the letter D and the vertices of the codomain digraph are marked by
#!  by the letter C.
DeclareOperation("DotWalkHomomorphism", [IsWalkHomomorphism]);

#! @Arguments T
#! @Returns a string
#! @Description
#!  Given an UDAF transducer, this operation returns a dot string for creating
#!  an image of the UDAF transducer. The vertices of the domain digraph are 
#!  marked by the letter D, the vertices of the codomain digraph are marked by
#!  the letter C, and the vertices of the underlying digraph of the transducer
#!  have no letter in their labels.
DeclareOperation("DotUDAFTransducer", [IsUDAFTransducer]);

#! @Arguments T, L
#! @Returns a string
#! @Description
#!  Given an UDAF transducer <A>T</A> and a list <A>L</A> with one entry for 
#!  each vertex of the underlying digraph of <A>T</A>, this operation returns
#!  a dot string for creating an image of the UDAF transducer. The vertices of 
#!  the domain digraph are marked by the letter D, the vertices of the codomain
#!  digraph are marked by the letter C, and the vertices of the underlying 
#!  digraph of the transducer are labelled according to the list <A>L</A>.
DeclareOperation("DotUDAFTransducerWithVertexLabels", [IsUDAFTransducer, IsDenseList]);

#! @Arguments T
#! @Returns 
#! @Description
#!  Generates an image of a walk homomorphism. This is a shortcut for the command
#!  Splash(DotWalkHomomorphism(<A>T</A>)).
DeclareOperation("Draw", [IsWalkHomomorphism]);

#! @Arguments T
#! @Returns 
#! @Description
#!  Given a shift isomorphim, this operation creates
#!  an image of a transducer defining the isomorphism. The UDAF transducer 
#!  chosen is a minimal UDAF transducer with the annotaion needed to define the 
#!  isomorphism. The vertices of the domain digraph are marked by the letter D,
#!  the vertices of the codomain digraph are marked by the letter C, and the 
#!  vertices of the underlying digraph of the transducer are lebelled in accordance
#!  with the annotation.
DeclareOperation("Draw", [IsShiftIsomorphism]);

#! @Arguments T
#! @Returns
#! @Description
#!  Given a shift isomorphim, this operation creates
#!  an image of a transducer defining the isomorphism. Both foldings of the 
#!  transducer will be synchronous two-sided foldings. The vertices of the domain
#!  digraph are marked by the letter D, the vertices of the codomain digraph are 
#!  marked by the letter C, and the vertices of the underlying digraph of the transducer
#!  have no letter in their labels.
DeclareOperation("DrawSynchronous", [IsShiftIsomorphism]);

#! @Arguments T
#! @Returns
#! @Description
#!  Given a one-sided shift isomorphim, this operation creates
#!  an image of a transducer defining the isomorphism. Both foldings of the 
#!  transducer will be synchronous one-sided foldings. The vertices of the domain
#!  digraph are marked by the letter D, the vertices of the codomain digraph are 
#!  marked by the letter C, and the vertices of the underlying digraph of the transducer
#!  have no letter in their labels.
DeclareOperation("Draw", [IsOneSidedShiftIsomorphism]);

#! @Arguments T
#! @Returns 
#! @Description
#!  Generates an image of an UDAF transducer. This is a shortcut for the command
#!  Splash(DotUDAFTransducer(<A>T</A>)).
DeclareOperation("Draw", [IsUDAFTransducer]);

#! @Arguments T
#! @Returns
#! @Description
#!  Given an UDAF isomorphism, this operation creates an image of the minimal
#!  transducer defining the isomorphism. The vertices of the domain
#!  digraph are marked by the letter D, the vertices of the codomain digraph are 
#!  marked by the letter C, and the vertices of the underlying digraph of the transducer
#!  have no letter in their labels.
DeclareOperation("Draw", [IsUDAFIsomorphism]);
