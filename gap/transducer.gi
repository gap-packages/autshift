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

  if not IsCoreTransducer(T) then
  ErrorNoReturn("aaa: ShiftMorphism: usage,\n",
                  "the transducer must be core,");
  fi;

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

InstallMethod(UDAFIsomorphism, "for a transducer",
[IsTransducer],
function(T)
  local M, basedigraph, domdigraph, codomdigraph, domfold, codomfold, state, 
        l, domedgemap, codomedgemap;

  if IsDegenerateTransducer(T) then
  ErrorNoReturn("autshift: UDAFIsomorphism: usage,\n",
                  "the transducer must not be degenerate,");
  fi;

  if not IsSynchronizingTransducer(T) then
  ErrorNoReturn("autshift: UDAFIsomorphism: usage,\n",
                  "the transducer must be sychronizing,");
  fi;

  if not IsCoreTransducer(T) then
  ErrorNoReturn("autshift: UDAFIsorphism: usage,\n",
                  "the transducer must be core,");
  fi;

  
  domdigraph := Digraph([List([1 .. NrInputSymbols(T)], x-> 1)]);
  codomdigraph := Digraph([List([1 .. NrOutputSymbols(T)], x-> 1)]);
  basedigraph := Digraph(TransitionFunction(T));
  domedgemap := [];
  codomedgemap :=[];
  for state in [1 .. NrStates(T)] do
    for l in [1 .. NrInputSymbols] do
      Add(domedgemap, [l]);
      Add(codomedgemap, List(OutputFunction(T)[state][l], x -> x+1));
    od;
  od;

  
  domfold := WalkHomomorphism(basedigraph, domdigraph, List(NrStates(T), x-> 1), domedgemap);
  codomfold := WalkHomomorphism(basedigraph, codomdigraph, List(NrStates(T), x-> 1), codomedgemap);

  if not IsUDAFFolding(codomfold) then
     ErrorNoReturn("autshift: UDAFIsorphism: usage,\n",
                  "the transducer must be UDAF invertible,");
  fi;

   M := Objectify(NewType(NewFamily("UDAFIsomorphism"), IsUDAFIsomorphism and
                 IsAttributeStoringRep), rec(Digraph:= basedigraph,
                                             DomainDigraph := domdigraph,
                                             CoDomainDigraph := codomdigraph,
                                             DomainFolding := domfold,
                                             CoDomainFolding := codomfold));

  return M;
end);


InstallMethod(UDAFIsomorphism, "for a transducer",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(f, g)
  local M, basedigraph, domdigraph, codomdigraph, domfold, codomfold, state,
        l, domedgemap, codomedgemap;

  if not IsUDAFFolding(f) and IsUDAFFolding(g) then
  ErrorNoReturn("autshift: UDAFIsorphism: usage,\n",
                  "the walk homomorphisms must be UDAF foldings,");
  fi;

  if not (DigraphVertices(f!.DomainDigraph) = DigraphVertices(g!.DomainDigraph) and 
          DigraphEdges(f!.DomainDigraph) = DigraphEdges(g!.DomainDigraph)) then
    ErrorNoReturn("autshift: UDAFIsorphism: usage,\n",
                  "the walk homomorphisms must have the same domain,");
  fi;

   M := Objectify(NewType(NewFamily("UDAFIsomorphism"), IsUDAFIsomorphism and
                 IsAttributeStoringRep), rec(Digraph:= f!.DomainDigraph,
                                             DomainDigraph := f!.CoDomainDigraph,
                                             CoDomainDigraph := g!.CoDomainDigraph,
                                             DomainFolding := f,
                                             CoDomainFolding := g));

  return M;
end);


InstallMethod(ComposeUDAFIsomorphisms, "for a pair of compatible UDAF Isomorphisms",
[IsUDAFIsomorphism, IsUDAFIsomorphism],
function(f, g)
  local Df2, Dg1, pastadder, futureadder, fmodifier, gmodifier, fpast, ffuture, gpast, gfuture;
  Df2 := f!.CoDomainDigraph;
  Dg1 := g!.DomainDigraph;

#note that this is stronger than insisting Df2 = Dg1 as it ensures the
#edges are stored in the same order
  if not [DigraphVertices(Df2), DigraphEdges(Df2)] = 
         [DigraphVertices(Dg1), DigraphEdges(Dg1)] then
     ErrorNoReturn("autshift: ComposeUDAFIsorphisms: usage,\n",
                  "the transducers must be composable,");
  fi;
  
  fmodifier := FoldingToLineFolding(f!.codomfold);
  gmodifier := FoldingToLineFolding(g!.domfold);

  fpast := MaxHistoryConeDepth(fmodifier[1]); 
  ffuture := MaxFutureConeDepth(fmodifier[1]);
  gpast := MaxHistoryConeDepth(gmodifier[1]);
  gfuture := MaxFutureConeDepth(gmodifier[1]);

  if fpast = -1 then
    return UDAFIsomorphism(WalkHomomorphism(EmptyDigraph(0), f!.DomainDigraph, [], []),
                           WalkHomomorphism(EmptyDigraph(0), g!.CoDomainDigraph, [], []));
  fi;

  if fpast < gpast then
    pastadder := LineDigraphWalkHomomorphism(fmodifier[1]!.DomainDigraph, gpast - fpast, 0);
    Apply(fmodifier, x-> pastadder * x);
  fi;
  if gpast < fpast then
    pastadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, fpast - gpast, 0);
    Apply(gmodifier, x-> pastadder * x);
  fi;
  if ffuture < gfuture then
    futureadder := LineDigraphWalkHomomorphism(fmodifier[1]!.DomainDigraph, 0, gfuture - ffuture);
    Apply(fmodifier, x-> futureadder * x);
  fi;
  if ffuture < gfuture then
    futureadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, 0, ffuture - gfuture);
    Apply(gmodifier, x-> futureadder * x);
  fi;

  return UDAFIsomorphism(fmodifier[2] * f!.DomainFolding, gmodifier[2] * g!.CoDomainFolding);
end);


InstallMethod(IdentityShiftMorphism, "for a positive integer",
[IsPosInt],
function(AlphSize)
  return ShiftMorphism(IdentityTransducer(AlphSize));
end);

InstallMethod(DeBruijnTransducer, "returns a transducer",
[IsPosInt, IsPosInt],
function(Alph, WordLen)
  local StateToLabel, LabelToState, state, letter, target, Pi, Lambda;
  StateToLabel := function(n)
     return List([0 .. (WordLen - 1)], x -> Int(RemInt(n - 1, Alph ^ (x + 1))
                                                / (Alph ^ x)));
  end;
  LabelToState := function(l)
    return 1 + Sum(List([0 .. WordLen - 1], y -> l[y + 1] * (Alph ^ y)));
  end;
  Pi := [];
  for state in [1 .. Alph ^ WordLen] do
    Add(Pi, []);
    for letter in [0 .. Alph - 1] do
      target := Concatenation(StateToLabel(state){[2 .. WordLen]}, [letter]);
      Add(Pi[state], LabelToState(target));
    od;
  od;
  Lambda := ListWithIdenticalEntries(Alph ^ WordLen,
                                     List([0 .. Alph - 1], x -> [x]));
  return Transducer(Alph, Alph, Pi, Lambda);
end);
