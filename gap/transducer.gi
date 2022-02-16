#############################################################################
##
#W  transducer.gi
#Y  Copyright (C) 2022                                 Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods that relate to the objects in this package,
# including appropiate ViewObj functions.

InstallMethod(ViewObj, "for a morphism of the shift",
[IsShiftMorphism],
function(T)
  local state, sym1, sym2;
  if T!.States = 1 then
    state := "state";
  else
    state := "states";
  fi;
  if T!.InputAlphabet = 1 then
    sym1 := "symbol";
  else
    sym1 := "symbols";
  fi;
  if T!.OutputAlphabet = 1 then
    sym2 := "symbol";
  else
    sym2 := "symbols";
  fi;

  Print("<morphism with input alphabet on ", T!.InputAlphabet, " ", sym1,
        ", output alphabet on ", T!.OutputAlphabet, " ", sym2, ", and ",
        T!.States, " ", state, ".>");
end);

InstallMethod(ShiftMorphism, "for a transducer",
[IsTransducer],
function(T)
  local M;

  if not IsSynchronizingTransducer(T) then
  ErrorNoReturn("aaa: ShiftMorphism: usage,\n",
                  "the transducer must be sychronizing,");
  fi;

  if not IsSynchronousTransducer(T) then
  ErrorNoReturn("aaa: ShiftMorphism: usage,\n",
                  "the transducer must be sychronous,");
  fi;
#TODO check IsCore

   M := Objectify(NewType(NewFamily("ShiftMorphism"), IsShiftMorphism and
                 IsAttributeStoringRep), rec(InputAlphabet := InputAlphabet(T),
                                             OutputAlphabet := OutputAlphabet(T),
                                             NrStates := NrStates(T),
                                             TransitionFunction := T!.TransitionFunction,
					     OutputFunction := OutputFunction(T),
                                             TransducerFunction := T!.TransducerFunction));

  return M;
end);

InstallMethod(IdentityShiftMorphism, "for a positive integer",
[IsPosInt],
function(AlphSize)
  return ShiftMorphism(IdentityTransducer(AlphSize));
end);


