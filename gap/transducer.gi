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
function(input)
  local state, sym1, sym2, T;
  T := input!.SynchronousUDAFTransducer;
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

  Print("<shift isomorphism whose domain digraph has ", DigraphNrEdges(T!.DomainDigraph), " ", sym1,
        ", whose codomain digraph has ", DigraphNrEdges(T!.CoDomainDigraph), " ", sym2, ", and which has ",
        DigraphNrVertices(T!.Digraph), " ", state, ".>");
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

InstallMethod(ViewObj, "for an UDAF isomorphism",
[IsUDAFIsomorphism],
function(T)
  local state, sym1, sym2;
  if DigraphNrVertices((T!.MinimalUDAFTransducer)!.Digraph) = 1 then
    state := "state";
  else
    state := "states";
  fi;
  if DigraphNrEdges((T!.MinimalUDAFTransducer)!.DomainDigraph) = 1 then
    sym1 := "edge";
  else
    sym1 := "edges";
  fi;
  if DigraphNrEdges((T!.MinimalUDAFTransducer)!.CoDomainDigraph) = 1 then
    sym2 := "edge";
  else
    sym2 := "edges";
  fi;

  Print("<UDAF Isomorphism whose domain digraph has ", 
        DigraphNrEdges((T!.MinimalUDAFTransducer)!.DomainDigraph), " ", sym1,
        ", whose codomain digraph has ", 
        DigraphNrEdges((T!.MinimalUDAFTransducer)!.CoDomainDigraph), " ", sym2, 
        ", and which has ",
        DigraphNrVertices((T!.MinimalUDAFTransducer)!.Digraph), " ", state, ".>");
end);

InstallMethod(ViewObj, "for an isomorphism of the shift",
[IsOneSidedShiftIsomorphism],
function(input)
  local state, sym1, sym2, T;
  T := input!.MinimalTransducer;
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

  Print("<one sided shift isomorphism whose domain digraph has ", DigraphNrEdges(T!.DomainDigraph), " ", sym1,
        ", whose codomain digraph has ", DigraphNrEdges(T!.CoDomainDigraph), " ", sym2, ", and which has ",
        DigraphNrVertices(T!.Digraph), " ", state, ".>");
end);

COMPUDAF := function(f, g, slow, trim)
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

  if not slow then
    f := f^-1;
    f := MinimalUDAFTransducer(f);
    f := f^-1;
  fi;
  
  
  fmodifier := FTOLFTRIM(f!.CoDomainFolding, trim);
  gmodifier := FTOLFTRIM(g!.DomainFolding, trim);

  fpast := MaxHistoryConeDepth(fmodifier[1]);
  gpast := MaxHistoryConeDepth(gmodifier[1]);
  
  if trim then
  ffuture := MaxFutureConeDepth(fmodifier[1]);
  gfuture := MaxFutureConeDepth(gmodifier[1]);
  fi;

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
  
  if trim then
  if ffuture < gfuture then
    futureadder := LineDigraphWalkHomomorphism(fmodifier[1]!.DomainDigraph, 0, gfuture - ffuture);
    fmodifier[1] := futureadder * fmodifier[1];
    fmodifier[2] := futureadder * fmodifier[2];
  fi;
  if gfuture < ffuture then
    futureadder := LineDigraphWalkHomomorphism(gmodifier[1]!.DomainDigraph, 0, ffuture - gfuture);
    gmodifier[1] := futureadder * gmodifier[1];
    gmodifier[2] := futureadder * gmodifier[2];

  fi;
  fi;

  return UDAFTransducer(fmodifier[2] * f!.DomainFolding, gmodifier[2] * g!.CoDomainFolding);
end;

IsValidAnnotatedTransducer := function(T, L)
  local edges, e;
  if not IsDeterministicWalkHomomorphism(T!.DomainFolding) then
    return false;
  fi;
  if not UDAFNrStates(T) = Size(L) then
    return false;
  fi;

  edges := DigraphEdges(T!.Digraph);
  for e in [1 .. Size(edges)] do
    if not L[edges[e][2]] - L[edges[e][1]] = Size((T!.CoDomainFolding)!.EdgeMap[e]) - 1 then
      return false;
    fi;
  od;
  return true;
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

  f := T!.DomainFolding;
  oldf := T!.DomainFolding;
  gvertexmap := [];
  gedgemap := [];
  R := DualWalkHomomorphism(f);
  oldR := DualWalkHomomorphism(oldf);
  oldfstatetopast := List([1 .. DigraphNrVertices(oldf!.DomainDigraph)],
                           x -> ShallowCopy(ImagesAsUnionsOfCones(R)[x][1]));
  for i in [1 .. Size(oldfstatetopast)] do
    oldfstatetopast[i][1] := dualedgewalktoedgewalk(oldf!.DomainDigraph, oldfstatetopast[i][1]);
  od;

  gdigraph := T!.CoDomainDigraph;

  gvertexinfo := List(DigraphVertices(gdigraph), x->[]);
  gedgeinfo := List(DigraphEdges(gdigraph), x->[]);
  # the next function is used in the loop following it
  # and assigns to each vertex/edge of the range digraph
  # a list of posssible past-future blocks which can be
  # used to deduce that the edge/vertex is to be written
  storeinfo := function(vertexoredge, position, info, isvertex)
    local i, output;
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
        Add(output[1], false);
      else
        Add(output[1], info[i]);
      fi;
    od;
    while Size(output[1]) > 0 and (output[1][1] = false) do
      Remove(output[1], 1); 
    od;
    while Size(output[3]) > 0 and (output[3][Size(output[3])] = false) do
      Remove(output[3], Size(output[3])); 
    od;
    if isvertex then
      AddSet(gvertexinfo[vertexoredge], output);
    fi;
    if not isvertex then
      AddSet(gedgeinfo[vertexoredge], output);
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
         dominfo := [oldfstatetopast[vertex][2]];
      else
         dominfo := edgesequecetoedgevertexsequence(f!.DomainDigraph, info);
      fi;
    position := Size(dominfo) + 2 * annotation[vertex];
    storeinfo(f!.VertexMap[vertex], position, dominfo, true);
    
    Append(dominfo, oldf!.EdgeMap[edge]);
    Add(dominfo, oldf!.VertexMap[DigraphEdges(f!.DomainDigraph)[edge][2]]);
    for i in [1 .. (Size(walk) - 1)/2] do
      position := position + 1;
      storeinfo(walk[2 * i], position, dominfo, false);
      position := position + 1;
      storeinfo(walk[2 * i + 1], position, dominfo, true);
    od;
  od;

  # we now use the information just computed to build the transducer
  neededpast := Maximum(List(Concatenation(gvertexinfo), x->Int(Size(x[1])/2)));
  neededfuture := Maximum(List(Concatenation(gvertexinfo), x->Int(Size(x[3])/2)));

  f := LineDigraphWalkHomomorphism(T!.DomainDigraph, neededpast, neededfuture);
  fstatetopastfuture := [];
  R := DualWalkHomomorphism(f);
  for i in [1 .. DigraphNrVertices(f!.DomainDigraph)] do
    block := [ShallowCopy(ImagesAsUnionsOfCones(R)[i][1][1]),
              f!.VertexMap[i],
              ShallowCopy(ImagesAsUnionsOfCones(f)[i][1][1])];
    if not block[1] = [] then
      block[1] := dualedgewalktoedgewalk(f!.CoDomainDigraph, block[1]);
      block[1] := edgesequecetoedgevertexsequence(f!.CoDomainDigraph, block[1]);
      Remove(block[1], Size(block[1]));
    fi;
    if not block[3] = [] then
      block[3] := edgesequecetoedgevertexsequence(f!.CoDomainDigraph, block[3]);
      Remove(block[3], 1);
    fi;
    Add(fstatetopastfuture, block);
  od;
  fedgetopastfuture := [];
  for e in [1 .. DigraphNrEdges(f!.DomainDigraph)] do
    pair := ShallowCopy(DigraphEdges(f!.DomainDigraph)[e]);
    pair[1] := Concatenation(fstatetopastfuture[pair[1]][1], 
                           [fstatetopastfuture[pair[1]][2]]);
    pair[2] := Concatenation([fstatetopastfuture[pair[2]][2]], 
                              fstatetopastfuture[pair[2]][3]);
    Add(fedgetopastfuture, [pair[1], f!.EdgeMap[e][1], pair[2]]);
  od;

  arecompatible := function(domthing, targetthing, isvertex)
    local flag;
    flag := false;
    if isvertex then
      domblock := fstatetopastfuture[domthing];
      targetblocks := gvertexinfo[targetthing];
    else
      domblock := fedgetopastfuture[domthing];
      targetblocks := gedgeinfo[targetthing];
    fi;
    for block in targetblocks do
      flag := false;
      if not ((block[2] = false) or (block[2] = domblock[2])) then
         flag := true;
      fi;
      for i in [1 .. Size(block[3])] do
        if (not block[3][i] = false) and ((i > Size(domblock[3])) or 
           (not (block[3][i] = domblock[3][i]))) then
          flag := true;
        fi;
      od;

      for i in [1 .. Size(block[1])] do
          if (not Reversed(block[1])[i] = false) and ((i > Size(domblock[1])) or 
             (not (Reversed(block[1])[i] = Reversed(domblock[1])[i]))) then
          flag := true;
        fi;
      od;
      if not flag then
        return true;
      fi;
    od;
    return false;
  end;

  gvertexmap := [];
  gedgemap := [];
  for state in [1 .. DigraphNrVertices(f!.DomainDigraph)] do
    for vertex in [1 .. DigraphNrVertices(T!.CoDomainDigraph)] do
      if arecompatible(state, vertex, true) then
        gvertexmap[state] := vertex;
        break;
      fi;
    od;
  od;
  for edge in [1 .. DigraphNrEdges(f!.DomainDigraph)] do
    for gedge in [1 .. DigraphNrEdges(f!.CoDomainDigraph)] do
      if arecompatible(edge, gedge, false) then
        gedgemap[edge] := [gedge];
        break;
      fi;
    od;
  od;


  g := WalkHomomorphism(f!.DomainDigraph, T!.CoDomainDigraph, gvertexmap, gedgemap);
  UDAFtrans := UDAFTransducer(f, g);
  if IsDeterministicWalkHomomorphism(f) then
     UDAFtrans := DeterministicDomainCombineEquivalentStates(UDAFtrans)[1];
  fi;
  if IsDeterministicWalkHomomorphism(UDAFtrans!.CoDomainFolding) then
     UDAFtrans := UDAFtrans^(-1);
     UDAFtrans := DeterministicDomainCombineEquivalentStates(UDAFtrans)[1];
     UDAFtrans := UDAFtrans^(-1);
  fi;
    
  M := Objectify(NewType(NewFamily("ShiftIsomorphism"), IsShiftIsomorphism and
                 IsAttributeStoringRep), rec(Digraph:= UDAFtrans!.Digraph,
                                             DomainDigraph := UDAFtrans!.DomainDigraph,
                                             CoDomainDigraph := UDAFtrans!.CoDomainDigraph,
                                             SynchronousUDAFTransducer := UDAFtrans,
                                             MinimalUDAFTransducer := MinUDAF,
                                             Annotation := annotation));

  return M;
end);


InstallMethod(UDAFNrStates, "for an UDAF transducer",
[IsUDAFTransducer],
function(T)
  return DigraphNrVertices(T!.Digraph);
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
  annotation := List(out[2], x-> Size(x[1]));
  out := SynchronousRemoveIncompleteResponse(f);
  f := out[1];
  annotation := List(annotation, x-> x - out[2]);
  out := DeterministicDomainCombineEquivalentStates(UDAFTransducer(f, g));
  annotation := List(out[2], x-> annotation[x[1]]);
  MinUDAF := out[1];

  M := Objectify(NewType(NewFamily("ShiftIsomorphism"), IsShiftIsomorphism and
                 IsAttributeStoringRep), rec(Digraph:= UDAFtrans!.Digraph,
                                             DomainDigraph := UDAFtrans!.DomainDigraph,
                                             CoDomainDigraph := UDAFtrans!.CoDomainDigraph,
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

InstallMethod(AreIsomorphicUDAFTransducers, "for a pair of UDAF transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(T1, T2)
   local L1, L2;
   L1 := List([1 .. UDAFNrStates(T1)], x-> 0);
   L2 := List([1 .. UDAFNrStates(T2)], x-> 0);
   return AreIsomorphicLabeledUDAFTransducers(T1, T2, L1, L2);
end);

#this insists that the domain and codomain digraphs are actually equal
#and that th
InstallMethod(AreIsomorphicLabeledUDAFTransducers, "for a pair of UDAF transducers",
[IsUDAFTransducer, IsUDAFTransducer, IsDenseList, IsDenseList],
function(T1, T2, L1, L2)
  local transducertovertexcoloureddigraph, D1, D2, colorings1, colorings2, 
        colorings3, colorings4, colors;
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

  colorings1 := List([1 .. UDAFNrStates(T1)], 
                                  x -> [(T1!.DomainFolding)!.VertexMap[x],
                                  (T1!.CoDomainFolding)!.VertexMap[x],
                                  L1[x],
                                  "this is a vertex"]);
  colorings2 := List([1 .. DigraphNrEdges(T1!.Digraph)], 
                               x -> [(T1!.DomainFolding)!.EdgeMap[x],
                              (T1!.CoDomainFolding)!.EdgeMap[x],
                              "this is an edge"]);
  colorings3 := List([1 .. UDAFNrStates(T2)], 
                    x -> [(T2!.DomainFolding)!.VertexMap[x],
                          (T2!.CoDomainFolding)!.VertexMap[x],
                          L2[x],
                        "this is a vertex"]);
  colorings4 := List([1 .. DigraphNrEdges(T2!.Digraph)], 
                     x -> [(T2!.DomainFolding)!.EdgeMap[x],
                     (T2!.CoDomainFolding)!.EdgeMap[x],
                      "this is an edge"]);
  if not (Set(colorings1) = Set(colorings3) and 
          Set(colorings2) = Set(colorings4)) then
    return false;
  fi;
  colors := Set(Concatenation(colorings1, colorings2, colorings3, colorings4));

  #we do this by converting the transducer into a directed digraph with
  #a vertex for each vertex or edge of the origional
  transducertovertexcoloureddigraph := function(T, L)
    local vvertices, vcolorings, evertices, ecolorings, vertices, colorings,
          adjcheck;
    vvertices := [1 .. UDAFNrStates(T)];
    vcolorings := List(vvertices, x -> [(T!.DomainFolding)!.VertexMap[x],
                                    (T!.CoDomainFolding)!.VertexMap[x],
                                    L[x],
                                    "this is a vertex"]);
    evertices := [UDAFNrStates(T) +  1 .. UDAFNrStates(T) + DigraphNrEdges(T!.Digraph)];
    ecolorings := List(evertices, x -> [(T!.DomainFolding)!.EdgeMap[x - UDAFNrStates(T)],
                                    (T!.CoDomainFolding)!.EdgeMap[x - UDAFNrStates(T)],
                                    "this is an edge"]);                          
    vertices := Concatenation(vvertices, evertices);
    colorings := Concatenation(vcolorings, ecolorings);
    Apply(colorings, x-> Position(colors, x));
    adjcheck := function(v, w)
      if v <= UDAFNrStates(T) then
        if w <= UDAFNrStates(T) then
          return false;
        else
          return v = DigraphEdges(T!.Digraph)[w - UDAFNrStates(T)][1];
        fi;
      else
        if w <= UDAFNrStates(T) then
          return w = DigraphEdges(T!.Digraph)[v - UDAFNrStates(T)][2];
        else
          return DigraphEdges(T!.Digraph)[v - UDAFNrStates(T)][2] =
                 DigraphEdges(T!.Digraph)[w - UDAFNrStates(T)][1];
        fi;
      fi;
      return DigraphEdges(T!.Digraph)[v][2] = DigraphEdges(T!.Digraph)[w][1];
    end;
    return [Digraph(vertices, adjcheck), colorings];
  end;
  D1 := transducertovertexcoloureddigraph(T1, L1);
  D2 := transducertovertexcoloureddigraph(T2, L2);
  return IsIsomorphicDigraph(D1[1], D2[1], D1[2], D2[2]);
end);

InstallMethod(UDAFIsomorphism, "for a transducer",
[IsUDAFTransducer],
function(T)
  local mintrans;
  mintrans := MinimalUDAFTransducer(T);
  return Objectify(NewType(NewFamily("UDAFIsomorphism"), IsUDAFIsomorphism and
                IsAttributeStoringRep), rec(Digraph:= mintrans!.Digraph,
                                            DomainDigraph := mintrans!.DomainDigraph,
                                            CoDomainDigraph := mintrans!.CoDomainDigraph,
                                            DomainFolding := mintrans!.DomainFolding,
                                            CoDomainFolding := mintrans!.CoDomainFolding,
                                            MinimalUDAFTransducer := mintrans));
end);

InstallMethod(UDAFIsomorphism, "for a transducer",
[IsTransducer],
function(T)
  return UDAFIsomorphism(UDAFTransducer(T));
end);

InstallMethod(UDAFIsomorphism, "for a transducer",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(f, g)
  return UDAFIsomorphism(UDAFTransducer(f, g));
end);

InstallMethod(UDAFIsomorphism, "for a transducer",
[IsShiftIsomorphism],
function(T)
  return UDAFIsomorphism(T!.MinimalUDAFTransducer);
end);

InstallMethod(UDAFTransducer, "for a transducer",
[IsTransducer],
function(T)
  local M, basedigraph, domdigraph, codomdigraph, domfold, codomfold, state,
        l, domedgemap, codomedgemap, temp, i;

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
    for l in [1 .. NrInputSymbols(T)] do
      Add(domedgemap, [l]);
      temp := OutputFunction(T)[state][l];
      if IsPeriodicList(temp) then
        temp := PrePeriod(temp);
      fi;
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
                              List([1 .. NrStates(T)], x-> 1),
                              domedgemap);
  codomfold := WalkHomomorphism(basedigraph,
                                codomdigraph,
                                List([1 .. NrStates(T)], x-> 1),
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
  local e, v, domfix, D, f, g,  x, EqRelation,
        i, tuple, NewTuple, b, flag, n, class, Classes,
         transitionbyedge,
        newvertices, newfvertexmap, newgvertexmap, newedges, newfedgemap,
        newgedgemap, newdigraph;

  f := T!.DomainFolding;
  g := T!.CoDomainFolding;
  domfix := FoldingToLineFolding(f);
  f := domfix[1];
  g := domfix[2] * g;
  f := SynchronousRemoveIncompleteResponse(f)[1];
  g :=  RemoveIncompleteResponse(g)[1];

  return DeterministicDomainCombineEquivalentStates(UDAFTransducer(f, g))[1];
end);

InstallMethod(DeterministicDomainCombineEquivalentStates, "for an UDAF Isomorphism",
[IsUDAFTransducer],
function(T)
  local e, v, domfix, D, f, g,  x, EqRelation, i, tuple, NewTuple, b, flag, n, 
        class, Classes, transitionbyedge, newvertices, 
        newfvertexmap, newgvertexmap, newedges, 
        newfedgemap, newgedgemap, newdigraph, domv, f1, f1v, f1img, buckets,
        f2, newbuckets, key, keytonumber, count;

  f := T!.DomainFolding;
  if not IsDeterministicWalkHomomorphism(f) then
    ErrorNoReturn("autshift: DeterministicDomainCombineEquivalentStates: usage,\n",
                 "the transducer's domain must be deterministic,");
  fi;

  g := T!.CoDomainFolding;

  transitionbyedge := function(v, e)
    local domedge;
    for domedge in OutEdgesAtVertex(T!.Digraph)[v] do
      if f!.EdgeMap[domedge[1]] = [e] then
        return domedge;
      fi;
    od;
  end;

  f1 := [];
  for v in DigraphVertices(T!.Digraph) do
    f1v := [, ];
    f1v[1] := [f!.VertexMap[v], g!.VertexMap[v]];
    f1v[2] := SortedList(List(OutEdgesAtVertex(T!.Digraph)[v], x-> [f!.EdgeMap[x[1]], g!.EdgeMap[x[1]]]));
    Add(f1, f1v);
  od;
  f1img := Set(f1);
  Apply(f1, x-> Position(f1img, x));
  buckets := List(f1img, x->HashSet(DigraphNrVertices(T!.Digraph)));
  for v in DigraphVertices(T!.Digraph) do
    AddSet(buckets[f1[v]], v);
  od;
  
  #this will stop via a break statement which occurs when we stop refining 
  #our partition
  flag := 10;
  while true do
    f2 := EmptyPlist(DigraphNrVertices(T!.Digraph));
    newbuckets := HashMap();
    for b in [1 .. Size(buckets)] do
      for v in buckets[b] do
        key := [b];
        for e in OutEdgesAtVertex(f!.CoDomainDigraph)[f!.VertexMap[v]] do
          Add(key, f1[transitionbyedge(v, e[1])[2]]);
        od;
        if not IsBound(newbuckets[key]) then
          newbuckets[key] := HashSet(1);
        fi;
        AddSet(newbuckets[key], v);
        f2[v] := key;     
      od;
    od;
    if Size(Set(f2)) = Size(Set(f1)) then
      break;
    fi;
    
    #work done for this iteration so we tidy the buckets
    #in prep for the next iteration
    buckets := [];
    keytonumber := HashMap();
    count := 0;
    for key in Keys(newbuckets) do
      count := count + 1;
      keytonumber[key] := count;
      Add(buckets, newbuckets[key]);
    od;
    for v in [1 .. Size(f2)] do
      f2[v] := keytonumber[f2[v]];
    od;
    f1 := f2;
  od;
  
  Classes := List(buckets, x-> Set(x));
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
    for e in OutEdgesAtVertex(f!.CoDomainDigraph)[newfvertexmap[v]] do
      domv := Classes[v][1];
      Add(newedges[Size(newedges)], class(transitionbyedge(domv, e[1])[2]));
      Add(newfedgemap, [e[1]]);
      Add(newgedgemap, g!.EdgeMap[transitionbyedge(domv, e[1])[1]]);
    od;
  od;

  newdigraph := Digraph(newedges);


  f := WalkHomomorphism(newdigraph, f!.CoDomainDigraph, newfvertexmap, newfedgemap);
  g := WalkHomomorphism(newdigraph, g!.CoDomainDigraph, newgvertexmap, newgedgemap);

  SetIsUDAFFolding(f, true);
  SetIsUDAFFolding(g, true);

  return [UDAFTransducer(f, g), Classes];
end);

InstallMethod(IdentityUDAFTransducer, "for an UDAF digraph",
[IsDigraph],
function(D)
  return UDAFTransducer(IdentityWalkHomomorphism(D), IdentityWalkHomomorphism(D));
end);


InstallMethod(IdentityShiftIsomorphism, "for an UDAF digraph",
[IsPosInt],
function(n)
  if not (n >1) then
    ErrorNoReturn("autshift: IdentityShiftIsomorphism: usage,\n",
                 "the integer must be at least 2,");
  fi;
  return ShiftIsomorphism(IdentityTransducer(n));
end);

InstallMethod(ComposeShiftIsomorphisms, "for a pair of Shift Isomorphisms",
[IsShiftIsomorphism, IsShiftIsomorphism],
function(f, g)
  return ShiftIsomorphism(COMPUDAF(
                          f!.SynchronousUDAFTransducer,
                          g!.SynchronousUDAFTransducer,
                          true, true));
end);


InstallMethod(ComposeUDAFTransducers, "for a pair of compatible UDAF Isomorphisms",
[IsUDAFTransducer, IsUDAFTransducer],
function(f, g)
  return COMPUDAF(f, g, false, true);
end);

InstallMethod(\*, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(f, g)
  return ComposeUDAFTransducers(f, g);
end);

InstallMethod(\*, "for a pair of compatible UDAF Transducers",
[IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism],
function(f, g)
  return ComposeOneSidedShiftIsomorphisms(f, g);
end);

InstallMethod(ComposeOneSidedShiftIsomorphisms, "for a pair of compatible one-sided shift isomorphisms",
[IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism],
function(f, g)
  return OneSidedShiftIsomorphism(
                COMPUDAF(f!.MinimalTransducer, 
                                           g!.MinimalTransducer, true, false));
end);

InstallMethod(\*, "for a pair of compatible UDAF Isomorphisms",
[IsUDAFIsomorphism, IsUDAFIsomorphism],
function(f, g)
  return ComposeUDAFIsomorphisms(f, g);
end);

InstallMethod(ComposeUDAFIsomorphisms, "for a pair of compatible UDAF isomorphisms",
[IsUDAFIsomorphism, IsUDAFIsomorphism],
function(f, g)
  return UDAFIsomorphism(f!.MinimalUDAFTransducer * g!.MinimalUDAFTransducer);
end);

InstallMethod(\*, "for a pair of compatible ShiftIsomorphisms",
[IsShiftIsomorphism, IsShiftIsomorphism],
function(f, g)
  return ComposeShiftIsomorphisms(f, g);
end);


AUTSHIFT_POW := function(T, n)
  if n = 1 then
    return T;
  fi;
  if n = -1 then
    if IsUDAFTransducer(T) then
      return UDAFTransducer(T!.CoDomainFolding, T!.DomainFolding);
    elif IsUDAFIsomorphism(T) then
      return UDAFIsomorphism(T!.MinimalUDAFTransducer ^ -1);
    elif IsShiftIsomorphism(T) then
      return ShiftIsomorphism(T!.SynchronousUDAFTransducer^(-1));
    elif IsOneSidedShiftIsomorphism(T) then
      return OneSidedShiftIsomorphism(T!.MinimalTransducer^(-1));
    fi;
  fi;
  if(DigraphVertices(T!.DomainDigraph) <> DigraphVertices(T!.CoDomainDigraph)) or
    (DigraphEdges(T!.DomainDigraph) <> DigraphEdges(T!.CoDomainDigraph)) then
    return fail;
  fi;
  if n = 0 then
    if IsUDAFTransducer(T) then
      return IdentityUDAFTransducer(T!.DomainDigraph);
    elif IsUDAFIsomorphism(T) then
      return UDAFIsomorphism(IdentityUDAFTransducer(T!.DomainDigraph));
    elif IsShiftIsomorphism(T) then
      return ShiftIsomorphism(IdentityUDAFTransducer(T!.DomainDigraph));
    elif IsOneSidedShiftIsomorphism(T) then
      return OneSidedShiftIsomorphism(IdentityUDAFTransducer(T!.DomainDigraph));
    fi;
  fi;
  if n < 0 then
    return (T^-1)^-n;
  fi;
  if IsOddInt(n) or n = 2 then
    return T^(n - 1) * T;
  fi;
  return (T^(n/2))^2;
end;

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsInt],
function(T, n)
  return AUTSHIFT_POW(T, n);
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsUDAFIsomorphism, IsInt],
function(T, n)
  return AUTSHIFT_POW(T, n);
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsShiftIsomorphism, IsInt],
function(T, n)
  return AUTSHIFT_POW(T, n);
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsOneSidedShiftIsomorphism, IsInt],
function(T, n)
  return AUTSHIFT_POW(T, n);
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism],
function(A, B)
  return B^-1 * A * B;
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsUDAFIsomorphism, IsUDAFIsomorphism],
function(A, B)
  return B^-1 * A * B;
end);

InstallMethod(\^, "for a pair of compatible UDAF Transducers",
[IsShiftIsomorphism, IsShiftIsomorphism],
function(A, B)
  return B^-1 * A * B;
end);

InstallMethod(\=, "for a pair of compatible UDAF Transducers",
[IsUDAFTransducer, IsUDAFTransducer],
function(T1, T2)
  return T1!.DomainFolding = T2!.DomainFolding and T1!.CoDomainFolding = T2!.CoDomainFolding;
end);

InstallMethod(\=, "for a pair of compatible UDAF Transducers",
[IsUDAFIsomorphism, IsUDAFIsomorphism],
function(T1, T2)
  return AreIsomorphicUDAFTransducers(T1!.MinimalUDAFTransducer, 
                                      T2!.MinimalUDAFTransducer);
end);


InstallMethod(\=, "for a pair of shift isomorphisms",
[IsShiftIsomorphism, IsShiftIsomorphism],
function(T1, T2)
  return AreIsomorphicLabeledUDAFTransducers(T1!.MinimalUDAFTransducer,
                                             T2!.MinimalUDAFTransducer,
                                             T1!.Annotation,
                                             T2!.Annotation);
end);

InstallMethod(\=, "for a pair of shift isomorphisms",
[IsOneSidedShiftIsomorphism, IsOneSidedShiftIsomorphism],
function(T1, T2)
  return AreIsomorphicUDAFTransducers(T1!.MinimalTransducer,
                                      T2!.MinimalTransducer);
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



# returns the transducer on alphabet out which reads blocks of length WordLen
# and writes in accordence with the function f
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


InstallMethod(OneSidedShiftIsomorphism, "for a pair of walk homomorphisms",
[IsUDAFTransducer],
function(T)
  return OneSidedShiftIsomorphism(T!.DomainFolding, T!.CoDomainFolding);
end);

BACK_TRIMING_UDAF_TRANSDUCER := function(T)
  local trim, f, g;
  trim := TRIMING_WALK_HOMOMORPHISMS(IdentityWalkHomomorphism(T!.Digraph), true, false);
  f := T!.DomainFolding;
  g := T!.CoDomainFolding;
  SetIsOneSidedFolding(trim, true);
  SetIsUDAFFolding(trim, true);
  return UDAFTransducer(trim * f, trim * g); 
end;

InstallMethod(OneSidedShiftIsomorphism, "for a pair of walk homomorphisms",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(f, g)
  local T, M;
  if not (IsOneSidedFolding(f) and IsOneSidedFolding(g)) then
    ErrorNoReturn("autshift: OneSidedShiftIsomorphism: usage,\n",
                 "the given walk homomorphisms must be one sided foldings,");
  fi;
  T := BACK_TRIMING_UDAF_TRANSDUCER(UDAFTransducer(f, g));
  T := DeterministicDomainCombineEquivalentStates(T)[1];
  M := Objectify(NewType(NewFamily("OneSidedShiftIsomorphism"), 
                   IsOneSidedShiftIsomorphism and
                   IsAttributeStoringRep), rec(Digraph:= T!.Digraph,
                                               DomainDigraph := T!.DomainDigraph,
                                               CoDomainDigraph := T!.CoDomainDigraph,
                                               MinimalTransducer := T));
  return M;
end);


#see https://arxiv.org/pdf/2004.08478v4.pdf page 27 for details of the following
#algorithm (in the case of a full shift)
NICE_TARGET_ONESIDED_TORSION := function(T0)
  local A, B, Bsync, A1, p, q, lambdap, lambdaq, alpha, e, temp, notdone, 
      edge1, edge2, i, h, newedgemap, j, newe, taualpha, R, HBiTA, nexth;
  #A1 Let T0 ∈ Hn. Let A and B be the underlying automata of T0 and T −10 respectively.
  A := (T0!.MinimalTransducer)!.DomainFolding;
  B := (T0!.MinimalTransducer)!.CoDomainFolding;
  
  #A2 If T0 has only one state, then it represents a permutation, and so there is a finite order single state
  #transducer that we can multiply against T0 to produce the identity element (in this case, go to the
  #final step of the algorithm with this finite order factor in hand). Otherwise, proceed to the next step.
  
  #instead of a single state we have whatever the target digraph is but by the same logic
  #we have an automorphism of that which must have finite order
  if A!.DomainDigraph = A!.CoDomainDigraph then
    return [T0];
  fi;

  #A3 Compute the synchronizing sequence (Bi)i∈N for B = B0.
  Bsync := SynchronizingSequence(B);

  #A4 Compute the first step A1 of the synchronizing sequence of A = A0.
  A1 := ReduceSynchronizingLength(A);

  #A5 Find a pair (p, q) of distinct states of A which belong to the same state of A1.
  for p in [1 .. DigraphNrVertices(A!.DomainDigraph)] do
    for q in [p + 1 .. DigraphNrVertices(A!.DomainDigraph)] do
      if A1[1]!.VertexMap[p] = A1[1]!.VertexMap[q] then
        break;
      fi;
    od;
    if A1[1]!.VertexMap[p] = A1[1]!.VertexMap[q] then
      break;
    fi;
  od;
  #note that because our target graph is assumed to be minimal, the vertices p, q
  #must have the same image under B as their images have the same outneighbours
  
  #A6 Find the non-identity permutation α of the output labels such that λ (·, q) ◦ α : Xn → Xn is precisely
  #λ (·, p) : Xn → Xn. Determine the disjoint cycle decomposition of the permutation α.
  lambdaq := List(OutEdgesAtVertex(B!.DomainDigraph)[q], x-> B!.EdgeMap[x[1]][1]);
  lambdap := List(OutEdgesAtVertex(B!.DomainDigraph)[p], x-> B!.EdgeMap[x[1]][1]);
  alpha := HashMap(Size(lambdaq));
  for e in [1 .. Size(lambdaq)] do
    alpha[lambdaq[e]] := lambdap[e];
  od;

  #A7 There is a smallest index i so that the state [q] of the automaton Bi has the following properties:
  #• The states [q] and [p] remain distinct states of Bi, and
  #• For all x, y ∈ Xn belonging to the same disjoint cycle in the cycle decomposition of α, the
  #edges labelled x and y from [q] are parallel edges.
  #Now determine the isomorphism τα of Bi which fixes all vertices and induces the permutation α
  #on the edges leaving [q].)
 
  #note that this i is also the largest i such that The states [q] and [p] remain distinct states of Bi
 
  i := 1;
  h := IdentityWalkHomomorphism(B!.DomainDigraph);
  nexth := h * SynchronizingSequenceConnections(B)[i];
  while nexth!.VertexMap[q] <>  nexth!.VertexMap[p]  do
    h := nexth;
    i := i+1;
    nexth := nexth * SynchronizingSequenceConnections(B)[i];
  od;

  newedgemap := List([1 .. DigraphNrEdges(SynchronizingSequence(B)[i]!.DomainDigraph)], x->[x]);
  for j in [1 .. DigraphNrEdges(SynchronizingSequence(B)[i]!.DomainDigraph)] do
    e := DigraphEdges(SynchronizingSequence(B)[i]!.DomainDigraph)[j];
    if e[1] = h!.VertexMap[q] then
      for newe in OutEdgesAtVertex(SynchronizingSequence(B)[i]!.DomainDigraph)[e[1]] do
        if e[2] = newe[2] and 
          SynchronizingSequence(B)[i]!.EdgeMap[newe[1]][1] = 
          alpha[SynchronizingSequence(B)[i]!.EdgeMap[j][1]] then
          newedgemap[j] := [newe[1]];
        fi;
      od;
    fi;
  od;
  taualpha := WalkHomomorphism(SynchronizingSequence(B)[i]!.DomainDigraph,
                       SynchronizingSequence(B)[i]!.DomainDigraph,
                       [1 .. DigraphNrVertices(SynchronizingSequence(B)[i]!.DomainDigraph)],
                       newedgemap);
  #A8 Build the transducer H(Bi, τα ). This is a factor of finite order in a product sequence that will
  #eventually trivialize T0.
  HBiTA := OneSidedShiftIsomorphism(SynchronizingSequence(B)[i], 
                           taualpha * SynchronizingSequence(B)[i]);

  #A9 Compute the product R = core(T ∗ H(Bi, τα )). This product has the same underlying graph as T
  #but is not minimal. The states corresponding to p and q in this product are ω-equivalent, and will
  #be identified by minimizing the result R to produce a new element T1 with fewer states than T0.
  R := T0 * HBiTA;

  #A10 Repeat this process from the beginning, remembering the list of finite factors found so far.
  return Concatenation(NICE_TARGET_ONESIDED_TORSION(R), [HBiTA^(-1)]);

  #A11 The transducer T now factors as the product in reverse order of the inverses of the finite order
  #factors found above.
end;


InstallMethod(OneSidedTorsionDecomposition, "for an automorphism of the one sided shift",
[IsOneSidedShiftIsomorphism],
function(A)
  local conjugator, ans;
  if A!.DomainDigraph <> A!.CoDomainDigraph then
    return fail;
  fi;
  conjugator := OneSidedShiftIsomorphism(IdentityWalkHomomorphism(A!.CoDomainDigraph), 
                                         OneSidedDigraphMinimise(A!.CoDomainDigraph));
                                         
  ans := List(NICE_TARGET_ONESIDED_TORSION(A^conjugator), x-> x^(conjugator^-1));
  if ans[1]=ans[1]^0 then
    Remove(ans, 1);
  fi;
  return ans;
end);

COMBINABLE_VERTICES := function(H)
    local i, j, pairs;
    pairs := [];
    for i in [1 .. DigraphNrVertices(H!.DomainDigraph)] do
      for j in [i + 1 .. DigraphNrVertices(H!.DomainDigraph)] do
        if OutNeighbours(H!.DomainDigraph)[i] = OutNeighbours(H!.DomainDigraph)[j] and
          List(OutEdgesAtVertex(H!.DomainDigraph)[i], x-> H!.EdgeMap[x[1]]) = 
          List(OutEdgesAtVertex(H!.DomainDigraph)[j], x-> H!.EdgeMap[x[1]]) then
          Add(pairs, [i, j]);
        fi;
      od;
    od;
    return Random(pairs);
end;


RANDOM_FINITE_ORDER_ONESIDED := function(D)
  local n, B, new, B2, vmap, emap, Autgrp, i, j, k, P1, P2, bound, emap2, temp,
        emap21, emap22;

  bound := Maximum(List(OutNeighbours(D), x-> Size(x)));
  n := 1;
  while Random(List([1 .. n], x-> (n >= (n - x)^2))) and (bound ^ n < 100) do
    n := n + 1;
  od;
  n := n - 1;


  B := LineDigraphWalkHomomorphism(D, n, 0);
  n := Random([0 .. Maximum(DigraphNrVertices(B!.DomainDigraph) - DigraphNrVertices(D), 0)]);
  i := 0;
  while (i < n or not IsMultiDigraph(B!.DomainDigraph)) and 
    (DigraphNrVertices(B!.DomainDigraph) > DigraphNrVertices(D)) do
    new := COMBINE_ALMOST_SYNCHRONISED_VERTICES_DIGRAPH(B!.DomainDigraph, COMBINABLE_VERTICES(B));
    B := MAKE_WALK_HOM(new!.CoDomainDigraph, D, 
                      List([1 .. DigraphNrVertices(new!.CoDomainDigraph)], x->B!.VertexMap[Position(new!.VertexMap, x)]),
                      List([1 .. DigraphNrEdges(new!.CoDomainDigraph)], x->B!.EdgeMap[Position(new!.EdgeMap, [x])]));
    i := i + 1;
  od;
  

  Autgrp := AutomorphismGroup(B!.DomainDigraph);
  if IsMultiDigraph(B!.DomainDigraph) then
    P1 := Projection(Autgrp, 1);
    P2 := Projection(Autgrp, 2);
    vmap := Random(List(Image(P1)));
    emap := Random(List(Image(P2)));
    while emap = IdentityTransformation and Random([1 .. 200]) > 1 do
      emap := Random(List(Image(P2)));
    od;
  else
    if Random([1 .. 50]) > 1 then
      return RANDOM_FINITE_ORDER_ONESIDED(D);
    fi;
    vmap := Random(List(Autgrp));
    emap := IdentityTransformation;
  fi;
  
#  emap2 := List([1 .. DigraphNrVertices(B!.DomainDigraph)], 
#                       x-> COPY(OutEdgesAtVertex(B!.DomainDigraph)[x^vmap]));
#  for i in [1 .. Size(emap2)] do
#    Apply(emap2[i], x-> [x[1], x[2]^vmap]);    
#  od;

#  emap2 := Concatenation(emap2);
  emap21 := List([1 .. DigraphNrEdges(B!.DomainDigraph)], 
                          x-> [DigraphEdges(B!.DomainDigraph)[x][1], 
                               DigraphEdges(B!.DomainDigraph)[x][2], 
                               x]);
  emap22 := List([1 .. DigraphNrEdges(B!.DomainDigraph)], 
                            x-> [DigraphEdges(B!.DomainDigraph)[x][1]^(vmap^(-1)), 
                                 DigraphEdges(B!.DomainDigraph)[x][2]^(vmap^(-1)), 
                                 x]);
  Sort(emap21);
  Sort(emap22);
  emap2 := HashMap(DigraphNrEdges(B!.DomainDigraph) + 1);
  for i in [1 .. Size(emap21)] do
    emap2[emap21[i][3]] := emap22[i][3]^emap;
  od;
  B2 := MAKE_WALK_HOM(B!.DomainDigraph,
                      B!.CoDomainDigraph,
                      List([1 .. DigraphNrVertices(B!.DomainDigraph)], 
                         x-> B!.VertexMap[x^vmap]),
                      List([1 .. DigraphNrEdges(B!.DomainDigraph)], 
                         x-> B!.EdgeMap[emap2[x]]));
  return OneSidedShiftIsomorphism(B, B2);
end;

InstallMethod(RandomOneSidedAut, "for an UDAF Digraph",
[IsDigraph],
function(D)
  local out;
  if not IsUDAFDigraph(D) then
    return fail;
  fi;
  out := RANDOM_FINITE_ORDER_ONESIDED(D);
  while Random([1 .. 3]) > 1 do
    out := out * RANDOM_FINITE_ORDER_ONESIDED(D);
  od;
  return out;
end);