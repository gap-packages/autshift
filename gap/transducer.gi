#########################################################################
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

InstallMethod(UDAFTransducer, "for a transducer",
[IsTransducer],
function(T)
  local M, basedigraph, domdigraph, codomdigraph, domfold, codomfold, state, 
        l, domedgemap, codomedgemap;

  if IsDegenerateTransducer(T) then
  ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
                  "the transducer must not be degenerate,");
  fi;

  if not IsSynchronizingTransducer(T) then
  ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
                  "the transducer must be sychronizing,");
  fi;

  if not IsCoreTransducer(T) then
  ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
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
     ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
                  "the transducer must be UDAF invertible,");
  fi;

   M := Objectify(NewType(NewFamily("UDAFTransducer"), IsUDAFTransducer and
                 IsAttributeStoringRep), rec(Digraph:= basedigraph,
                                             DomainDigraph := domdigraph,
                                             CoDomainDigraph := codomdigraph,
                                             DomainFolding := domfold,
                                             CoDomainFolding := codomfold));

  return M;
end);


InstallMethod(UDAFTransducer, "for a transducer",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(f, g)
  local M, basedigraph, domdigraph, codomdigraph, domfold, codomfold, state,
        l, domedgemap, codomedgemap;

  if not IsUDAFFolding(f) and IsUDAFFolding(g) then
  ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
                  "the walk homomorphisms must be UDAF foldings,");
  fi;

  if not (DigraphVertices(f!.DomainDigraph) = DigraphVertices(g!.DomainDigraph) and 
          DigraphEdges(f!.DomainDigraph) = DigraphEdges(g!.DomainDigraph)) then
    ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
                  "the walk homomorphisms must have the same domain,");
  fi;

   M := Objectify(NewType(NewFamily("UDAFTransducer"), IsUDAFTransducer and
                 IsAttributeStoringRep), rec(Digraph:= f!.DomainDigraph,
                                             DomainDigraph := f!.CoDomainDigraph,
                                             CoDomainDigraph := g!.CoDomainDigraph,
                                             DomainFolding := f,
                                             CoDomainFolding := g));

  return M;
end);


InstallMethod(MinimiseUDAFTransducer, "for an UDAF Isomorphism",
[IsUDAFTransducer],
function(t)
  local Tedgesstartingwithvertex, T, e, v, domfix, D, f, g,  x, EqRelation, 
        i, tuple, NewTuple, b, flag, n, class, Classes, 
        fedgesstartingwithvertex, compatiblevertexpair, transitionbyedge,
        newvertices, newfvertexmap, newgvertexmap, newedges, newfedgemap,
        newgedgemap, newdigraph; 

  f := t!.DomainFolding;
  g := t!.CoDomainFolding;
  domfix := FoldingToLineFolding(f);
  f := domfix[1];
  g := domfix[2] * g;
  f := SynchronousRemoveIncompleteResponse(f);
  g :=  RemoveIncompleteResponse(g);


  T := UDAFTransducer(f, g);  

  Tedgesstartingwithvertex := [];
  e := 1;
  for v in DigraphVertices(T!.Digraph) do
     Add(Tedgesstartingwithvertex, []);
     for n in OutNeighbours(T!.Digraph)[v] do
       Add(Tedgesstartingwithvertex[Size(Tedgesstartingwithvertex)], e);
       e:= e + 1;
     od;
  od;

  compatiblevertexpair := function(p)
    local outedgeinfo1, outedgeinfo2;
    if not f!.VertexMap[p[1]] = f!.VertexMap[p[2]] then
      return false;
    fi; 
    if not g!.VertexMap[p[1]] = g!.VertexMap[p[2]] then
      return false;
    fi;
    outedgeinfo1 := List(Tedgesstartingwithvertex[p[1]], x-> [f!.EdgeMap[x], g!.EdgeMap[x]]);
    outedgeinfo2 := List(Tedgesstartingwithvertex[p[2]], x-> [f!.EdgeMap[x], g!.EdgeMap[x]]);
    Sort(outedgeinfo1);
    Sort(outedgeinfo2);
    return outedgeinfo1 = outedgeinfo2;
  end;

  fedgesstartingwithvertex := [];
  e := 1;
  for v in DigraphVertices(f!.CoDomainDigraph) do
     Add(fedgesstartingwithvertex, []);
     for n in OutNeighbours(f!.DoDomainDigraph)[v] do
       Add(fedgesstartingwithvertex[Size(fedgesstartingwithvertex)], e);
       e:= e + 1;
     od;
  od;

  transitionbyedge := function(v, e)
    local domedge;
    for domedge in Tedgesstartingwithvertex[v] do
      if f!.EdgeMap(domedge) = [e] then
        return [domedge, DigraphEdges(T)[domedge][2]];
      fi;
    od;
  end;

  EqRelation := Filtered(UnorderedTuples(DigraphVertices(T!.Digraph), 2), compatiblevertexpair);
  flag := true;
  while flag do
    flag := false;
    for tuple in EqRelation do
      for i in fedgesstartingwithvertex[f!.VertexMap[tuple[1]]] do
        NewTuple := [transitionbyedge(tuple[1], i)[2],
                     transitionbyedge(tuple[2], i)[2]];
        Sort(NewTuple);
        if not NewTuple in EqRelation then
          Remove(EqRelation, Position(EqRelation,tuple));
          flag := true;
          break;
        fi;
      od;
    od;
  od;

  Classes := ShallowCopy(EquivalenceRelationPartition(EquivalenceRelationByPairs(Domain(DigraphVertices(T!.Digraph)), EqRelation)));
  class := function(q)
        local j;
        for j in [1 .. Length(Classes)] do
                if q in Classes[j] then
                        return j;
                fi;
        od;
  end;

  newvertices := [1 .. Size(Classes)];
  newfvertexmap := List(newvertices, x-> f!.VertexMap[Classes[x][1]]);
  newgvertexmap := List(newvertices, x-> g!.VertexMap[Classes[x][1]]);

  newedges := [];
  newfedgemap := [];
  newgedgemap := [];
  for v in newvertices do
    Add(newedges, []);
    for e in fedgesstartingwithvertex[newfvertexmap[v]] do
      Add(newedges[Size(newedges)], class(transitionbyedge(v, e)[2]));
      Add(newfedgemap, [e]);
      Add(newgedgemap, g!.EdgeMap[transitionbyedge(v, e)[1]]);
    od;
  od;

  newdigraph := Digraph(newedges);


  f := WalkHomomorphism(newdigraph, f!.CoDomainDigraph, newfvertexmap, newfedgemap);
  g := WalkHomomorphism(newdigraph, g!.CoDomainDigraph, newgvertexmap, newgedgemap);

  return Transducer(f, g);
end);


InstallMethod(ComposeUDAFTransducers, "for a pair of compatible UDAF Isomorphisms",
[IsUDAFTransducer, IsUDAFTransducer],
function(f, g)
  local Df2, Dg1, pastadder, futureadder, fmodifier, gmodifier, fpast, ffuture, gpast, gfuture;
  Df2 := f!.CoDomainDigraph;
  Dg1 := g!.DomainDigraph;

#note that this is stronger than insisting Df2 = Dg1 as it ensures the
#edges are stored in the same order
  if not [DigraphVertices(Df2), DigraphEdges(Df2)] = 
         [DigraphVertices(Dg1), DigraphEdges(Dg1)] then
     ErrorNoReturn("autshift: ComposeUDAFTransducer: usage,\n",
                  "the transducers must be composable,");
  fi;

  if f!.CoDomainFolding = g!.DomainFolding then
    return UDAFTransducer(f!.DomainFolding, g!.CoDomainFolding);
  fi;
  
  fmodifier := FoldingToLineFolding(f!.codomfold);
  gmodifier := FoldingToLineFolding(g!.domfold);

  fpast := MaxHistoryConeDepth(fmodifier[1]); 
  ffuture := MaxFutureConeDepth(fmodifier[1]);
  gpast := MaxHistoryConeDepth(gmodifier[1]);
  gfuture := MaxFutureConeDepth(gmodifier[1]);

  if fpast = -1 then
    return UDAFTransducer(WalkHomomorphism(EmptyDigraph(0), f!.DomainDigraph, [], []),
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

  return UDAFTransducer(fmodifier[2] * f!.DomainFolding, gmodifier[2] * g!.CoDomainFolding);
end);

InstallMethod(\*, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(f, g)
  return ComposeUDAFTransducers(f, g);
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsInt],
function(T, n)
  if not n = -1 then
    return fail;
  fi;
  return UDAFTransducer(T!.CoDomainFolding, T!.DomainFolding);
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
