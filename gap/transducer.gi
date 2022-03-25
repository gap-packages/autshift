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

InstallMethod(ViewObj, "for an isomorphism of the shift",
[IsShiftIsomorphism],
function(T)
  ViewObj(T!.SynchronousUDAFTransducer);
end);

InstallMethod(ViewObj, "for an UDAF transducer",
[IsUDAFTransducer],
function(T)
  local state, sym1, sym2;
  if DigraphNrVertices(T!.Digraph) = 1 then
    state := "state";
  else
    state := "states";
  fi;
  if DigraphNrEdges(T!.DomainDigraph) = 1 then
    sym1 := "edge";
  else
    sym1 := "edges";
  fi;
  if DigraphNrEdges(T!.CoDomainDigraph) = 1 then
    sym2 := "edge";
  else
    sym2 := "edges";
  fi;

  Print("<UDAF Transducer whose domain digraph has ", DigraphNrEdges(T!.DomainDigraph), " ", sym1,
        ", whose codomain digraph has ", DigraphNrEdges(T!.CoDomainDigraph), " ", sym2, ", and which has ",
        DigraphNrVertices(T!.Digraph), " ", state, ".>");
end);


# this function convertes a non-empty walk
# given as a list of edges to the corresponding
# vertex->edge->vertex->edge->vertex ... walk
edgesequecetoedgevertexsequence := function(D, edges)
  local out;
  out := List(edges, x-> [DigraphEdges(D)[x][1], x]);
  Add(out, [DigraphEdges(D)[edges[Size(edges)]][2]]);
  return Concatenation(out);
end;


# this function convertes an edge walk in the dual of a digraph
# to the corresponding edge walk
dualedgewalktoedgewalk := function(D, walk)
   local edgeperm, i;
   edgeperm := List([1 .. DigraphNrEdges(D)], x->[ShallowCopy(DigraphEdges(D)[x]), x]);
   for i in [1 .. DigraphNrEdges(D)] do
     edgeperm[i][1] := [edgeperm[i][1][2], edgeperm[i][1][1]];
   od;
   Sort(edgeperm);
   edgeperm := List(edgeperm, x-> x[2]);
   return Reversed(List(walk, x-> Position(edgeperm, x)));
end;

#this could be made way faster
InstallMethod(IsMinimalUDAFTransducer, "for an UDAF transducer",
[IsUDAFTransducer],
function(T)
  return AreIsomorphicUDAFTransducers(T, MinimalUDAFTransducer(T));
end);

InstallMethod(ShiftIsomorphism, "for a transducer",
[IsUDAFTransducer, IsDenseList],
function(T, annotation)
  local M, UDAFtrans, MinUDAF, out, oldf, f, g, oldfstatetopast, storeinfo,
        oldR, R, gvertexinfo, gedgeinfo, walk, gvertexmap, gedgemap, i,
        gdigraph, output, vertex, block, e, edge, info, dominfo, position,
        domblock, pair, neededfuture, neededpast, fstatetopastfuture,
        fedgetopastfuture, fvertextopastfuture, arecompatible, targetblocks,
        state, gedge;

  if not IsValidAnnotatedTransducer(T, annotation) then
    return ErrorNoReturn("autshift: ShiftIsomorphism: usage,\n",
                          "input is invalid,");
  fi;

  if not IsMinimalUDAFTransducer(T) then
    return ErrorNoReturn("autshift: ShiftIsomorphism: usage,\n",
                          "input transducer must be minimal,");
  fi;

  MinUDAF := T;
  UDAFtrans := MinUDAF;


  oldf := T!.DomainFolding;
  gvertexmap := [];
  gedgemap := [];
  R := DualWalkHomomorphism(f);
  oldR := DualWalkHomomorphism(oldf);
  oldfstatetopast := List([1 .. DigraphNrVertices(oldf!.DomainDigraph)],
                           x -> ImageAsUnionOfCones(R, x)[1]);
  for i in [1 .. Size(oldfstatetopast)] do
    oldfstatetopast[i][1] := dualedgewalktoedgewalk(oldf!.DomainDigraph, oldfstatetopast[i][1]);
  od;

  gdigraph := T!.CoDomainDigraph;

  gvertexinfo := List(DigraphVertices(gdigraph), x->[]);
  gedgeinfo := List(DigraphVertices(gdigraph), x->[]);
  # the next two functions are used in this loop following them
  # and assign to each vertex/edge of the range digraph
  # a list of posssible past-future blocks which can be
  # used to deduce that the edge/vertex is to be written
  storeinfo := function(vertexoredge, position, info, isvertex)
    output := [[], [], []];
    if position in [1 .. Size(info)] then
      output[2] := info[position];
    else
      output[2] := false;
    fi;
    for i in [position + 1 .. Size(info)] do
      if i < 1 then
        Add(output[3], false);
      else
        Add(output[3], info[i]);
      fi;
    od;
    for i in [1 .. position - 1] do
      if i > Size(info) then
        Add(output[3], false);
      else
        Add(output[1], info[i]);
      fi;
    od;

    if isvertex then
      Add(gvertexinfo[vertexoredge], output);
    fi;
    if not isvertex then
      Add(gedgeinfo[vertexoredge], output);
    fi;
  end;

  for edge in [1 .. DigraphNrEdges(f!.DomainDigraph)] do
    walk := f!.EdgeMap[edge];
    if walk = [] then
       walk := [f!.VertexMap[DigraphEdges[f!.DomainDigraph[edge]][1]]];
    else
       walk := edgesequecetoedgevertexsequence(f!.DomainDigraph, walk);
    fi;
    vertex := DigraphEdges(f!.DomainDigraph)[edge][1];
    dominfo := oldfstatetopast[vertex][1];
      if dominfo = [] then
         dominfo := oldfstatetopast[vertex][2];
      else
         dominfo := edgesequecetoedgevertexsequence(f!.DomainDigraph, info);
      fi;
    position := Size(info) + 2 * annotation[vertex];
    storeinfo(f!.VertexMap[vertex], position, info, true);

    Add(dominfo, oldf!.Edgemap[edge]);
    Add(dominfo, oldf!.Vertexmap[DigraphEdges(f!.DomainDigraph)[edge][2]]);
    for i in [2 .. (Size(walk) - 1)/2] do
      position := position + 1;
      storeinfo(walk[2 * i], position, info, false);
      position := position + 1;
      storeinfo(walk[2 * i + 1], position, info, true);
    od;
  od;

  # we now use the information just computed to build the transducer
  neededpast := Maximum(List(Concatenation(gvertexinfo)), x->Size(x[1])/2);
  neededfuture := Maximum(List(Concatenation(gvertexinfo)), x->Size(x[3])/2);

  f := LineDigraphWalkHomomorphism(T!.DomainDigraph, neededpast, neededfuture);
  fstatetopastfuture := [];
  for i in [1 .. DigraphNrVertices(f!.DomainDigraph)] do
    block := [ImageAsUnionOfCones(R, i)[1][1],
              f!.VetexMap[i],
              ImageAsUnionOfCones(f, i)[1][1]];
    if not block[1] = [] then
      block[1] := dualedgewalktoedgewalk(f!.DomainDigraph, block[1]);
      block[1] := edgesequecetoedgevertexsequence(f!.DomainDigraph, block[1]);
      Remove(block[1], Size(block[1]));
    fi;
    if not block[3] = [] then
      block[3] := edgesequecetoedgevertexsequence(f!.DomainDigraph, block[3]);
      Remove(block[3], 1);
    fi;
    Add(fstatetopastfuture, block);
  od;
  fedgetopastfuture := [];
  for e in [1 .. DigraphNrEdges(f!.DomainDigraph)] do
    pair := DigraphEdges(f!.DomainDigraph)[e];
    Add(fedgetopastfuture, [fvertextopastfuture[pair[1]][1],
                             f!.EdgeMap[e][1],
                             fvertextopastfuture[pair[2]][3]]);
  od;

  arecompatible := function(domthing, targetthing, isvertex)
    if isvertex then
      domblock := fstatetopastfuture[domthing];
      targetblocks := gvertexinfo[targetthing];
    else
      domblock := fvertextopastfuture[domthing];
      targetblocks := gedgeinfo[targetthing];
    fi;
    for block in targetblocks do
      if not block[2] = domblock[2] then
         continue;
      fi;
      for i in [1 .. Size(block[3])] do
        if not ((block[3][i] = domblock[3][i]) or (block[3][i] = false)) then
          continue;
        fi;
      od;

      for i in [1 .. Size(block[1])] do
        if not ((Reversed(block[1])[i] = Reversed(domblock[1])[i]) or
                (Reversed(block[1])[i] = false)) then
          continue;
        fi;
      od;
      return true;
    od;
    return false;
  end;

  gvertexmap := [];
  gedgemap := [];
  for state in f!.DomainDigraph do
    for vertex in T!.CoDomainDigraph do
      if arecompatible(state, vertex, true) then
        gedgemap[state] := vertex;
        break;
      fi;
    od;
  od;
  for edge in f!.DomainDigraph do
    for gedge in T!.CoDomainDigraph do
      if arecompatible(edge, gedge, false) then
        gedgemap[edge] := [gedge];
        break;
      fi;
    od;
  od;


  g := LineDigraphWalkHomomorphism(f!.DomainDigraph, T!.CoDomainDigraph, gvertexmap, gedgemap);

  M := Objectify(NewType(NewFamily("ShiftIsomorphism"), IsShiftIsomorphism and
                 IsAttributeStoringRep), rec(Digraph:= UDAFtrans!.Digraph,
                                             DomainDigraph := UDAFtrans!.DomainDigraph,
                                             CoDomainDigraph := UDAFtrans!.CoDomainDigraph,
                                             DomainFolding := UDAFtrans!.DomainFolding,
                                             CoDomainFolding := UDAFtrans!.CoDomainFolding,
                                             SynchronousUDAFTransducer := UDAFtrans,
                                             MinimalUDAFTransducer := MinUDAF,
                                             Annotation := annotation));

  return M;
end);


InstallMethod(ShiftIsomorphism, "for a synchronous UDAF transducer",
[IsUDAFTransducer],
function(T)
  local M, UDAFtrans, MinUDAF, annotation, out, f, g, domfix,
        LineDomainUDAFtrans;

  if not (IsSynchronousWalkHomomorphism(T!.DomainFolding) and 
         IsSynchronousWalkHomomorphism(T!.CoDomainFolding)) then
  ErrorNoReturn("autshift: ShiftIsomorphism: usage,\n",
                  "the transducer must be synchronous,");
  fi;

  UDAFtrans := T;

  f := UDAFtrans!.DomainFolding;
  g := UDAFtrans!.CoDomainFolding;
  domfix := FoldingToLineFolding(f);
  f := domfix[1];
  g := domfix[2] * g;
  LineDomainUDAFtrans := UDAFTransducer(f, g);

  out :=  RemoveIncompleteResponse(g);
  g := out[1];
  annotation := List(out[2], x-> -Size(x[1]));
  out := SynchronousRemoveIncompleteResponse(f);
  f := out[1];
  annotation := List(annotation, x-> x + out[2]);
  out := SynchronousDomainCombineEquivalentStates(UDAFTransducer(f, g));
  annotation := List(out[2], x-> annotation[x[1]]);
  MinUDAF := out[1];

  M := Objectify(NewType(NewFamily("ShiftIsomorphism"), IsShiftIsomorphism and
                 IsAttributeStoringRep), rec(Digraph:= UDAFtrans!.Digraph,
                                             DomainDigraph := UDAFtrans!.DomainDigraph,
                                             CoDomainDigraph := UDAFtrans!.CoDomainDigraph,
                                             DomainFolding := UDAFtrans!.DomainFolding,
                                             CoDomainFolding := UDAFtrans!.CoDomainFolding,
                                             SynchronousUDAFTransducer := UDAFtrans,
                                             MinimalUDAFTransducer := MinUDAF,
                                             Annotation := annotation));

  return M;
end);

InstallMethod(ShiftIsomorphism, "for a transducer",
[IsTransducer],
function(T)
  return ShiftIsomorphism(UDAFTransducer(T));

end);

#this insists that the domain and codomain digraphs are actually equal
#and that th
InstallMethod(AreIsomorphicUDAFTransducers, "for a pair of UDAF transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(T1, T2)
  local transducertovertexcoloureddigraph, D1, D2;
  #Note that this is stronger than checking if the digraphs are equal due
  #to edge ordering
  if not (DigraphNrVertices(T1!.DomainDigraph) = DigraphNrVertices(T2!.DomainDigraph)
         and DigraphEdges(T1!.DomainDigraph) = DigraphEdges(T2!.DomainDigraph)) then
    return false;
  fi;
  if not (DigraphNrVertices(T1!.CoDomainDigraph) = DigraphNrVertices(T2!.CoDomainDigraph)
         and DigraphEdges(T1!.CoDomainDigraph) = DigraphEdges(T2!.CoDomainDigraph)) then
    return false;
  fi;

  #part of an old algorith with didn't work
  #we check the isomrophism by converting the transducers into vertex coloured
  #digraphs with a vertex for each edge. this ignores the isolated vertics
  #so we deal with those separatly
  #OutN1 := DigraphOutNeighbours(T1!.Digraph);
  #InN1 := DigraphInNeighbours(T1!.Digraph);
  #OutN2 := DigraphOutNeighbours(T1!.Digraph);
  #InN2 := DigraphInNeighbours(T1!.Digraph);
  #lonelyvertices1 := Filtered([1 .. UDAFNrStates(T1)], x-> (OutN1 = [] and InN1 = []));
  #lonelyvertices2 := Filtered([1 .. UDAFNrStates(T2)], x-> (OutN2 = [] and InN2 = []));
  #Apply(lonelyvertices1, x - > T1!.VertexMap[x]);
  #Apply(lonelyvertices2, x - > T2!.VertexMap[x]);
  #Sort(lonelyvertices1);
  #Sort(lonelyvertices2);
  #if not lonelyvertices1 = lonelyvertices2 then
  #  return false;
  #fi;

  #we do this by converting the transducer into a directed digraph with
  #a vertex for each vertex or edge of the origional
  transducertovertexcoloureddigraph := function(T)
    local vvertices, vcolorings, evertices, ecolorings, vertices, colorings,
          colors, adjcheck;
    vvertices := [1 .. UDAFNrStates(T)];
    vcolorings := List(vvertices, x -> [(T!.DomainFolding)!.VertexMap[x],
                                    (T!.CoDomainFolding)!.VertexMap[x],
                                    "this is a vertex"]);
    evertices := [UDAFNrStates(T) +  1 .. UDAFNrStates(T) + DigraphNrEdges(T!.Domain)];
    ecolorings := List(evertices, x -> [(T!.DomainFolding)!.EdgeMap[x - UDAFNrStates(T)],
                                    (T!.CoDomainFolding)!.EdgeMap[x - UDAFNrStates(T)],
                                    "this is an edge"]);
    vertices := Concatenation(vvertices, evertices);
    colorings := Concatenation(vcolorings, ecolorings);
    colors := Set(colorings);
    Apply(colorings, x-> Position(colors, x));
    adjcheck := function(v, w)
      if v <= UDAFNrStates(T) then
        if w <= UDAFNrStates(T) then
          return false;
        else
          return v = DigraphEdges(T!.Domain)[w - UDAFNrStates(T)][1];
        fi;
      else
        if w <= UDAFNrStates(T) then
          return w = DigraphEdges(T!.Domain)[v - UDAFNrStates(T)][2];
        else
          return DigraphEdges(T!.Domain)[v - UDAFNrStates(T)][2] =
                 DigraphEdges(T!.Domain)[w - UDAFNrStates(T)][1];
        fi;
      fi;
      return DigraphEdges(T!.Domain)[v][2] = DigraphEdges(T!.Domain)[w][1];
    end;
    return [Digraph(vertices, adjcheck), colorings];
  end;
  D1 := transducertovertexcoloureddigraph(T1);
  D2 := transducertovertexcoloureddigraph(T2);
  return IsIsomorphicDigraph(D1[1], D2[1], D1[2], D2[2]);
end);

InstallMethod(UDAFTransducer, "for a transducer",
[IsTransducer],
function(T)
  local M, basedigraph, domdigraph, codomdigraph, domfold, codomfold, state,
        l, domedgemap, codomedgemap, temp;

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
  for state in [1 .. UDAFNrStates(T)] do
    for l in [1 .. NrInputSymbols(T)] do
      Add(domedgemap, [l]);
      temp := OutputFunction(T)[state][l];
      if not Size(temp) < infinity then
        ErrorNoReturn("autshift: UDAFTransducer: usage,\n",
                  "the transducer must be UDAF invertible,");
      fi;
      temp := List([1 .. Size(temp)], x -> temp[x] + 1);
      Add(codomedgemap, temp);
    od;
  od;


  domfold := WalkHomomorphism(basedigraph,
                              domdigraph,
                              List([1 .. UDAFNrStates(T)], x-> 1),
                              domedgemap);
  codomfold := WalkHomomorphism(basedigraph,
                                codomdigraph,
                                List([1 .. UDAFNrStates(T)], x-> 1),
                                codomedgemap);

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

  if not (IsUDAFFolding(f) and IsUDAFFolding(g)) then
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

InstallMethod(MinimalUDAFTransducer, "for an UDAF Isomorphism",
[IsUDAFTransducer],
function(T)
  local Tedgesstartingwithvertex, e, v, domfix, D, f, g,  x, EqRelation,
        i, tuple, NewTuple, b, flag, n, class, Classes,
        fedgesstartingwithvertex, compatiblevertexpair, transitionbyedge,
        newvertices, newfvertexmap, newgvertexmap, newedges, newfedgemap,
        newgedgemap, newdigraph;


  f := T!.DomainFolding;
  g := T!.CoDomainFolding;
  domfix := FoldingToLineFolding(f);
  f := domfix[1];
  g := domfix[2] * g;
  f := SynchronousRemoveIncompleteResponse(f);
  g :=  RemoveIncompleteResponse(g);

  return DeterministicDomainCombineEquivalentStates(UDAFTransducer(f, g))[1];
end);

InstallMethod(DeterministicDomainCombineEquivalentStates, "for an UDAF Isomorphism",
[IsUDAFTransducer],
function(T)
  local Tedgesstartingwithvertex, e, v, domfix, D, f, g,  x, EqRelation,
        i, tuple, NewTuple, b, flag, n, class, Classes,
        fedgesstartingwithvertex, compatiblevertexpair, transitionbyedge,
        newvertices, newfvertexmap, newgvertexmap, newedges, newfedgemap,
        newgedgemap, newdigraph;

  f := T!.DomainFolding;
  if not IsDeterministicWalkHomomorphism(f) then
    ErrorNoReturn("autshift: DeterministicDomainCombineEquivalentStates: usage,\n",
                 "the transducer's domain must be deterministic,");
  fi;

  g := T!.CoDomainFolding;

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
     for n in OutNeighbours(f!.CoDomainDigraph)[v] do
       Add(fedgesstartingwithvertex[Size(fedgesstartingwithvertex)], e);
       e:= e + 1;
     od;
  od;

  transitionbyedge := function(v, e)
    local domedge;
    for domedge in Tedgesstartingwithvertex[v] do
      if f!.EdgeMap[domedge] = [e] then
        return [domedge, DigraphEdges(T!.Digraph)[domedge][2]];
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
        Add(Classes, [q]);
        return(Size(Classes));
  end;

  for i in DigraphVertices(T!.Digraph) do
    class(i);
  od;
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

  return [UDAFTransducer(f, g), Classes];
end);

InstallMethod(IdentityUDAFTransducer, "for an UDAF digraph",
[IsDigraph],
function(D)
  return UDAFTransducer(IdentityWalkHomomorphism(D), IdentityWalkHomomorphism(D));
end);

InstallMethod(ComposeShiftIsomorphisms, "for a pair of Shift Isomorphisms",
[IsShiftIsomorphism, IsShiftIsomorphism],
function(f, g)
  local Df2, Dg1, pastadder, futureadder, fmodifier, gmodifier, fpast, ffuture, gpast, gfuture;
  Df2 := f!.CoDomainDigraph;
  Dg1 := g!.DomainDigraph;

  #note that this is stronger than insisting Df2 = Dg1 as it ensures the
  #edges are stored in the same order
  if not [DigraphVertices(Df2), DigraphEdges(Df2)] =
         [DigraphVertices(Dg1), DigraphEdges(Dg1)] then
     ErrorNoReturn("autshift: ComposeShiftIsomorphisms: usage,\n",
                  "the transducers must be composable,");
  fi;

  if f!.CoDomainFolding = g!.DomainFolding then
    return UDAFTransducer(f!.DomainFolding, g!.CoDomainFolding);
  fi;

  fmodifier := FoldingToLineFolding(f!.CoDomainFolding);
  gmodifier := FoldingToLineFolding(g!.DomainFolding);

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
    fmodifier[1] := pastadder * fmodifier[1];
    fmodifier[2] := pastadder * fmodifier[2];
  fi;
  if gpast < fpast then
    pastadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, fpast - gpast, 0);
    gmodifier[1] := pastadder * gmodifier[1];
    gmodifier[2] := pastadder * gmodifier[2];
  fi;
  if ffuture < gfuture then
    futureadder := LineDigraphWalkHomomorphism(fmodifier[1]!.DomainDigraph, 0, gfuture - ffuture);
    fmodifier[1] := futureadder * fmodifier[1];
    fmodifier[2] := futureadder * fmodifier[2];
  fi;
  if gfuture < ffuture then
    futureadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, 0, ffuture - gfuture);
    gmodifier[1] := pastadder * gmodifier[1];
    gmodifier[2] := pastadder * gmodifier[2];

  fi;

  return UDAFTransducer(fmodifier[2] * f!.DomainFolding, gmodifier[2] * g!.CoDomainFolding);

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

  fmodifier := FoldingToLineFolding(f!.CoDomainFolding);
  gmodifier := FoldingToLineFolding(g!.DomainFolding);

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
    fmodifier[1] := pastadder * fmodifier[1];
    fmodifier[2] := pastadder * fmodifier[2];
  fi;
  if gpast < fpast then
    pastadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, fpast - gpast, 0);
    gmodifier[1] := pastadder * gmodifier[1];
    gmodifier[2] := pastadder * gmodifier[2];
  fi;
  if ffuture < gfuture then
    futureadder := LineDigraphWalkHomomorphism(fmodifier[1]!.DomainDigraph, 0, gfuture - ffuture);
    fmodifier[1] := futureadder * fmodifier[1];
    fmodifier[2] := futureadder * fmodifier[2];
  fi;
  if gfuture < ffuture then
    futureadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, 0, ffuture - gfuture);
    gmodifier[1] := pastadder * gmodifier[1];
    gmodifier[2] := pastadder * gmodifier[2];

  fi;

  return UDAFTransducer(fmodifier[2] * f!.DomainFolding, gmodifier[2] * g!.CoDomainFolding);
end);

InstallMethod(\*, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(f, g)
  return ComposeUDAFTransducers(f, g);
end);

InstallMethod(\*, "for a pair of compatible UDAF Transducers",
[IsShiftIsomorphism, IsShiftIsomorphism],
function(f, g)
  return ComposeShiftIsomorphisms(f, g);
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsInt],
function(T, n)
  if not n = -1 then
    return fail;
  fi;
  return UDAFTransducer(T!.CoDomainFolding, T!.DomainFolding);
end);


InstallMethod(\=, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(T1, T2)
  return T1!.DomainFolding = T2!.DomainFolding and T1!.CoDomainFolding = T2!.CoDomainFolding;
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

InstallMethod(UDAFNrStates, "for an UDAF transducer",
[IsUDAFTransducer],
function(T)
  return DigraphNrVertices(T!.Digraph);
end);

InstallMethod(IsValidAnnotatedTransducer, "for an UDAF transducer and a dense list",
[IsUDAFTransducer, IsDenseList],
function(T, L)
  local edges, e;
  if not IsDeterministicWalkHomomorphism(T!.DomainFolding) then
    return false;
  fi;
  if not UDAFNrStates(T) = Size(L) then
    return false;
  fi;

  edges := DigraphEdges(T!.Digraph);
  for e in [1 .. Size(edges)] do
    if not L[edges[e][2]] - L[edges[e][1]] = Size((T!.CoDomainFolding)!.EdgeMap[e]) then
      return false;
    fi;
  od;
  return true;
end);


# returns the transducer on alphabet out which reads blocks of length WordLen
# and write in accordence with the function f
InstallMethod(BlockCodeTransducer, "for a positive integer and a block code function", [IsPosInt, IsInt, IsFunction],
function(Alph, WordLen, f)
  local StateToLabel, LabelToState, state, letter, target, Pi, Lambda;
  if Alph < 2 then
     ErrorNoReturn("autshift: BlockCodeTransducer: usage,\n",
                  "the alphabet must have at least two letters,");
  fi;
  StateToLabel := function(n)
     return List([0 .. (WordLen - 1)], x -> Int(RemInt(n - 1, Alph ^ (x + 1))
                                                / (Alph ^ x)));
  end;
  LabelToState := function(l)
    return 1 + Sum(List([0 .. WordLen - 1], y -> l[y + 1] * (Alph ^ y)));
  end;
  Pi := [];
  Lambda := [];
  for state in [1 .. Alph ^ WordLen] do
    Add(Pi, []);
    Add(Lambda, []);
    for letter in [0 .. Alph - 1] do
      target := Concatenation(StateToLabel(state){[2 .. WordLen]}, [letter]);
      Add(Pi[state], LabelToState(target));
      Add(Lambda[state], f(Concatenation(StateToLabel(state), [letter])));
    od;
  od;
  return Transducer(Alph, Alph, Pi, Lambda);
end);



InstallMethod(ResizeZeroStringTransducer, "for three two positive integers", [IsPosInt, IsPosInt, IsPosInt],
function(AlphSize, i, j)
  local itoj, count, B;

  itoj := function(word)
    count := 0;
    if ForAll(word, x -> x = 0) then
      return [0];
    elif word[Size(word)] <> 0 then
      while count < Size(word) - 1 do
        if word[Size(word)-count - 1] = 0 then
          count := count + 1;
        else
          break;
        fi;
      od;
      if count in [i, j] and word[Size(word)] = 1 then
        count := i + j - count;
      fi;
      return Concatenation(ListWithIdenticalEntries(count, 0),
                           [word[Size(word)]]);
    else
      return [];
    fi;
  end;

  B := BlockCodeTransducer(AlphSize, Maximum(i, j) + 1, itoj);

  return TransducerCore(MinimalTransducer(B));
end);
