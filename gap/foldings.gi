#############################################################################
##
#W  foldings.gi
#Y  Copyright (C) 2022                                 Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods that relate to the objects in this package,
# including appropiate ViewObj functions.

InstallMethod(WalkHomomorphism, "for a digraph, a digraph, a list, and a list",
[IsDigraph, IsDigraph, IsDenseList, IsDenseList],
function(D1, D2, LV, LE)
  local D1Vert, D1Edge, D2Vert, D2Edge, M, iswalk;

  D1Vert := DigraphVertices(D1);
  D1Edge := DigraphEdges(D1);
  D2Vert := DigraphVertices(D2);
  D2Edge := DigraphEdges(D2);

  if not Size(LV) = Size(D1Vert) then
  ErrorNoReturn("AutShift: WalkHomomorphism: usage,\n",
                  "the third input must have one entry for each vertex of\n",
                  " the first input,");
  fi;
  if not Size(LE) = Size(D1Edge) then
  ErrorNoReturn("AutShift: WalkHomomorphism: usage,\n",
                  "the fourth input must have one entry for each edge of\n",
                  " the first input,");
  fi;

  if not ForAll(LV, x -> x in D2Vert) then
  ErrorNoReturn("AutShift: WalkHomomorphism: usage,\n",
                  "the third input must be a list of vertices of the second\n",
                  "input,");
  fi;

  iswalk := function(L)
     if L = [] then
       return true;
     fi;
     if not ForAll(L, x-> x in [1 .. Size(D2Edge)]) then
       return false;
     fi;
     if not ForAll([1 .. Size(L) - 1], x-> D2Edge[L[x]][2] =  D2Edge[L[x + 1]][1]) then
       return false;
     fi;
     return true;     
  end;

  if not ForAll(LE, x -> iswalk(x)) then
      ErrorNoReturn("AutShift: WalkHomomorphism: usage,\n",
                  "the fourth input must be a list of edge walks in the second\n",
                  "input,");
  fi;
  if not ForAll([1 .. Size(LE)], x -> (LE[x] = []) or 
         (D2Edge[LE[x][1]][1] = LV[D1Edge[x][1]] and D2Edge[LE[x][Size(LE[x])]][2] = LV[D1Edge[x][2]])) then
      ErrorNoReturn("AutShift: WalkHomomorphism: usage,\n",
                  "the walks in the forth input must be compatible with\n",
                  " the vertices in the third input,");
  fi;

  M := Objectify(NewType(NewFamily("WalkHomomorphism"), IsWalkHomomorphism and IsAttributeStoringRep), 
                                             rec(DomainDigraph := D1,
                                             CoDomainDigraph := D2,
                                             VertexMap := LV,
                                             EdgeMap := LE));
  return M;
end);



InstallMethod(ViewObj, "for a walk homomorphism",
[IsWalkHomomorphism],
function(H)
  local domletters, targetletters;
  if DigraphNrEdges(H!.DomainDigraph) = 1 then
    domletters := "edge";
  else
    domletters := "edges";
  fi;
  if DigraphNrEdges(H!.CoDomainDigraph) = 1 then
    targetletters := "edge";
  else
    targetletters := "edges";
  fi;

  Print("<walk homomorphism from a digraph with ", DigraphNrEdges(H!.DomainDigraph), " ", domletters,
        " to a digraph with ",DigraphNrEdges(H!.CoDomainDigraph), " ", targetletters, ".>");
end);


InstallMethod(ComposeWalkHomomorphisms, "for a pair of walk homomorphisms",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(H1, H2)
  local newLV, newLE, oldLV1, oldLV2, oldLE1, oldLE2;
  oldLV1 := H1!.VertexMap;
  oldLV2 := H2!.VertexMap;
  oldLE1 := H1!.EdgeMap;
  oldLE2 := H2!.EdgeMap;
  newLV := List(oldLV1, x -> oldLV2[x]);
  newLE := List(oldLE1, x -> Concatenation(List([1 .. Size(x)], y->oldLE2[x[y]])));
  return WalkHomomorphism(H1!.DomainDigraph, H2!.CoDomainDigraph, newLV, newLE);
end);

InstallMethod(IdentityWalkHomomorphism, "for a digraph",
[IsDigraph],
function(D)
  return WalkHomomorphism(D, D, DigraphVertices(D), List([1 .. DigraphNrEdges(D)], x->[x]));
end);


InstallMethod(IsDegenerateWalkHomomorphism, "for a WalkHomomorphism",
[IsWalkHomomorphism],
function(H)
  local emptyedgedigraph, edges, v;
  edges := [1 .. Size(H!.EdgeMap)];
  edges := Filtered(edges, x-> H!.EdgeMap[x] = []);
  edges := List(edges, x-> DigraphEdges(H!.DomainDigraph)[x]);
  v := Size(H!.VertexMap);
  emptyedgedigraph := Digraph(v, List(edges, x->x[1]), List(edges, x->x[2]));
  if DigraphHasLoops(emptyedgedigraph) then
    return true;
  fi;
  if DigraphGirth(emptyedgedigraph) < infinity then
    return true;
  fi;
  return false;
end);

InstallMethod(IsUDAFDigraph, "for a digraph",
[IsDigraph],
function(input)
  local badverticesf, checkedverticesf, checkbadf, checkbadb, badverticesb, 
        checkedverticesb, v, D;
  D := TrimWalkHomomorphism(IdentityWalkHomomorphism(input))!.DomainDigraph;
  checkedverticesf := List(DigraphVertices(D), x-> false);
  badverticesf := List(DigraphVertices(D), x-> false);
  checkbadf := function(v, currentdepth)
    if currentdepth > DigraphNrVertices(D) then
      checkedverticesf[v] := true;
      badverticesf[v] := true;
      return true;
    fi;
    if checkedverticesf[v] then
      return badverticesf[v];
    fi;
    if not Size(OutNeighbours(D)[v])= 1 then
      checkedverticesf[v] := true;
      badverticesf[v] := false;
      return false;
    fi;

    badverticesf[v] := checkbadf(OutNeighbours(D)[v][1], currentdepth + 1);
    return badverticesf[v];
  end;
  

  checkedverticesb := List(DigraphVertices(D), x-> false);
  badverticesb := List(DigraphVertices(D), x-> false);
  checkbadb := function(v, currentdepth)
    if currentdepth > DigraphNrVertices(D) then
      checkedverticesb[v] := true;
      badverticesb[v] := true;
      return true;
    fi;
    if checkedverticesb[v] then
      return badverticesb[v];
    fi;
    if not Size(InNeighbours(D)[v])= 1 then
      checkedverticesb[v] := true;
      badverticesb[v] := false;
      return false;
    fi;

    badverticesb[v] := checkbadb(InNeighbours(D)[v][1], currentdepth + 1);
    return badverticesb[v];
  end;

  for v in DigraphVertices(D) do
    if checkbadb(v, 0) or checkbadf(v, 0) then
      return false;
    fi;
  od;

  return true;
end);

#given H:D1->D2", returns a synchronous H':D3->D2, and an UDAF folding
#f:D3-> D1 such that fH and H' induce the same function from the unindexed routes of D3 to the
#unindexed routes of D2
InstallMethod(SynchronousWalkHomomorphism, "for a digraph, a digraph, a list, and a list",
[IsWalkHomomorphism],
function(H)
  local SWH;
  if IsDegenerateWalkHomomorphism(H) then
     ErrorNoReturn("AutShift: SynchronousWalkHomomorphism: usage,\n",
                "the given homomorphism must be non-degenerate");
  fi;
  if not IsUDAFDigraph(H!.CoDomainDigraph) then
     ErrorNoReturn("AutShift: SynchronousWalkHomomorphism: usage,\n",
                "the target digraph must be an UDAF Digraph");
  fi;


  SWH := function(H, f)
    local newhom, BadEdge, oldedges, i, domvno, oldedgesandimage, 
          newedgesandimage, out, EMa, vtarg, conversionvtarg, conversionhom;
 
    BadEdge := 0;
    domvno := DigraphNrVertices(H!.DomainDigraph);
    EMa := H!.EdgeMap;
    oldedges :=DigraphEdges(H!.DomainDigraph);
    oldedgesandimage := List([1 .. Size(oldedges)], x->[ShallowCopy(oldedges[x]), EMa[x], [x]]);
    for i in [1 .. Size(EMa)] do
      if EMa[i] = [] then
        BadEdge := i;
        break;
      fi;
    od;

    if BadEdge = 0 then
      for i in [1 .. Size(EMa)] do
        if Size(EMa[i]) > 1 then
          BadEdge := i;
          break;
        fi;
      od;
      if BadEdge = 0 then
        return [H, f];
      fi;

      #list of edge pairs together with the path the edge maps to and what number it is
      newedgesandimage := Concatenation(oldedgesandimage, [ [[domvno + 1, oldedges[BadEdge][2]], [EMa[BadEdge][Size(EMa[BadEdge])]], []] ]);
      conversionvtarg := DigraphEdges(H!.DomainDigraph)[BadEdge][2];
      vtarg := DigraphEdges(H!.CoDomainDigraph)[Remove(newedgesandimage[BadEdge][2])][1];
      newedgesandimage[BadEdge][1][2] := domvno + 1;
      Sort(newedgesandimage);
      newhom := WalkHomomorphism(Digraph(domvno + 1, 
                                         List(newedgesandimage, x->x[1][1]), 
                                         List(newedgesandimage, x->x[1][2])),
                             H!.CoDomainDigraph,
                             Concatenation(H!.VertexMap, [vtarg]),
                             List(newedgesandimage, x->x[2]));

      conversionhom := WalkHomomorphism(Digraph(domvno + 1,
                                         List(newedgesandimage, x->x[1][1]),
                                         List(newedgesandimage, x->x[1][2])),
                             H!.DomainDigraph,
                             Concatenation(ShallowCopy(DigraphVertices(H!.DomainDigraph)), [conversionvtarg]),
                             List(newedgesandimage, x -> x[3]));

      return SWH(newhom, conversionhom * f);
    fi;

    out := Filtered([1 .. Size(oldedges)], x->oldedges[x][1] = oldedges[BadEdge][2]);
    List(out, x -> [[oldedges[BadEdge][1], oldedges[x][2]], EMa[x]]);
    newedgesandimage := Concatenation(oldedgesandimage, List(out, x -> [[oldedges[BadEdge][1], oldedges[x][2]], EMa[x], [BadEdge, x]]));
    Remove(newedgesandimage, BadEdge);
    Sort(newedgesandimage);
  
    newhom := WalkHomomorphism(Digraph(domvno, 
                                     List(newedgesandimage, x->x[1][1]), 
                                     List(newedgesandimage, x->x[1][2])), 
                             H!.CoDomainDigraph, 
                             H!.VertexMap, 
                             List(newedgesandimage, x->x[2]));
    conversionhom := WalkHomomorphism(Digraph(domvno,
                                         List(newedgesandimage, x->x[1][1]),
                                         List(newedgesandimage, x->x[1][2])),
                             H!.DomainDigraph,
                             ShallowCopy(DigraphVertices(H!.DomainDigraph)),
                             List(newedgesandimage, x->x[3]));
    return SWH(newhom, conversionhom * f);

  end;
  return SWH(H, IdentityWalkHomomorphism(H!.DomainDigraph));
end);



InstallMethod(R2toPhiFold, "",
[],
function()
  local R2, Phi, Vmap, Emap;
  R2 := Digraph([[1, 1]]);
  Phi := Digraph([[1, 2], [1]]);
  Vmap := [1];
  Emap := [[1], [2, 3]];
  return WalkHomomorphism(R2, Phi, Vmap, Emap);
end);


InstallMethod(PhitoR2Fold, "",
[],
function()  
  local R2, Phi, Vmap, Emap;
  R2 := Digraph([[1, 1]]);
  Phi := Digraph([[1, 2], [1]]);
  Vmap := [1, 1];
  Emap := [[1], [2], []];
  return WalkHomomorphism(Phi, R2, Vmap, Emap);
end);



InstallMethod(WalkHomomorphismVertexImageAutomaton, "",
[IsWalkHomomorphism, IsInt],
function(H, v)
  local alphabet, EdgeMap, e, D, i, j, k, transitiontable, currentnewstate, numberofstates; 
  EdgeMap := H!.EdgeMap;
  D := H!.DomainDigraph;
  numberofstates := DigraphNrVertices(H!.DomainDigraph);
  alphabet := DigraphNrEdges(H!.CoDomainDigraph);
  if not v in [1 .. numberofstates] then
     ErrorNoReturn("AutShift: WalkHomomorphismVertexImageAutomaton: usage,\n",
                  "the given integer must be a vertex of the doman digraph");
  fi;
  for i in EdgeMap do
    if not Size(i) = 0 then
      numberofstates := numberofstates + Size(i) - 1;
    fi;
  od;

  transitiontable := List([1 ..  alphabet + 1],
                           x -> List([1 .. numberofstates], y-> []));
  currentnewstate := DigraphNrVertices(H!.DomainDigraph) + 1;
  e := 1;
  for i in DigraphVertices(H!.DomainDigraph) do
    for j in [1 .. Size(OutNeighbours(D)[i])] do
      if Size(EdgeMap[e]) > 1 then
         Add(transitiontable[EdgeMap[e][1]][i], currentnewstate);
         for k in [2 .. Size(EdgeMap[e]) - 1] do
           AddSet(transitiontable[EdgeMap[e][k]][currentnewstate], currentnewstate + 1);
           currentnewstate := currentnewstate + 1;
         od;
           AddSet(transitiontable[EdgeMap[e][Size(EdgeMap[e])]][currentnewstate], OutNeighbours(D)[i][j]);
           currentnewstate := currentnewstate + 1;
      fi;
      if Size(EdgeMap[e]) = 1 then
          AddSet(transitiontable[EdgeMap[e][1]][i], OutNeighbours(D)[i][j]);
      fi;
      if Size(EdgeMap[e]) < 1 then
          AddSet(transitiontable[alphabet + 1][i], OutNeighbours(D)[i][j]);
      fi;
      e := e + 1;
    od;
  od;
  return Automaton("epsilon", numberofstates,
                   Concatenation(List([1 .. alphabet], x -> String(x)[1]),"@"),
                   transitiontable, [v], [1 .. numberofstates]);
end);


InstallMethod(IsUDAFFolding, "a walk homomorphism",
[IsWalkHomomorphism],
function(H)
   if IsDegenerateWalkHomomorphism(H) or not IsUDAFDigraph(H!.CoDomainDigraph) then
     return false;
   fi;
   return not FoldingToLineFolding(H) = fail;
end);



InstallMethod(FoldingToLineFolding, "for a walk homomorphism which is a folding",
[IsWalkHomomorphism],
function(H)
  local sync, S, f2, R, e1, e2, v, new, out1, Rout1, outforwardedges, outbackwardedges, 
        out2vertexmap, out2edgemap, vertexforwardimages, vertexbackwardimages, futureinfo,
        historyinfo, outforwardimages, outbackwardimages, containedincone;
  sync := SynchronousWalkHomomorphism(H);
  Apply(sync, TrimWalkHomomorphism);
  S := sync[1];
  f2 := sync[2];
  R := DualWalkHomomorphism(S);

  if DigraphNrVertices(S!.DomainDigraph) = 0 then
    if DigraphNrVertices(TrimWalkHomomorphism(IdentityWalkHomomorphism(S!.CoDomainDigraph))!.DomainDigraph) = 0 then
      return [S, f2];
    fi;
    return fail;
  fi;

  for e1 in [1 .. DigraphNrEdges(S!.DomainDigraph)] do
    for e2 in [1 .. DigraphNrEdges(S!.DomainDigraph)] do
      if DigraphEdges(S!.DomainDigraph)[e1] = DigraphEdges(S!.DomainDigraph)[e2] and
         S!.EdgeMap[e1] = S!.EdgeMap[e2] and not (e1 = e2) then
         return fail;
      fi;
    od;
  od;

  vertexforwardimages := List(DigraphVertices(S!.DomainDigraph), x-> ImageAsUnionOfCones(S, x));
  vertexbackwardimages := List(DigraphVertices(R!.DomainDigraph), x-> ImageAsUnionOfCones(R, x));

  if ForAny(vertexforwardimages, x-> x= fail) or ForAny(vertexbackwardimages, x-> x=fail) then
    return fail;
  fi;

  futureinfo := Maximum(List(Concatenation(vertexforwardimages), x-> Size(x[1])));
  historyinfo := Maximum(List(Concatenation(vertexbackwardimages), x-> Size(x[1])));

  out1 := LineDigraphWalkHomomorphism(S!.CoDomainDigraph, historyinfo, futureinfo);
  Rout1 := DualWalkHomomorphism(out1);
  outforwardimages := List(DigraphVertices(out1!.DomainDigraph), x-> ImageAsUnionOfCones(out1, x));
  outbackwardimages := List(DigraphVertices(Rout1!.DomainDigraph), x-> ImageAsUnionOfCones(Rout1, x));

  containedincone := function(deepcone, shallowcone, reversed)
     if not shallowcone[1] = [] then
       return IsPrefix(deepcone[1], shallowcone[1]);
     fi;
     if deepcone[1] = [] then
       return shallowcone[2] = deepcone[2];
     fi;
     if reversed then
       return shallowcone[2] = DigraphEdges(R!.DomainDigraph)[deepcone[1][1]][1];
     fi;
     return shallowcone[2] = DigraphEdges(S!.DomainDigraph)[deepcone[1][1]][1];
  end;

  out2vertexmap := [];
  for v in DigraphVertices(out1!.DomainDigraph) do
    new := Filtered([1 .. DigraphNrVertices(S!.DomainDigraph)], 
              x-> ForAny(vertexforwardimages[x], y-> containedincone(outforwardimages[v][1], y, false))
              and ForAny(vertexbackwardimages[x], y-> containedincone(outbackwardimages[v][1], y, true)));
    if not Size(new) = 1 then
      return fail;
    fi;
    new := new[1];
    Add(out2vertexmap, new);
  od;

  out2edgemap := [];
  for e1 in [1 .. DigraphNrEdges(out1!.DomainDigraph)] do
    for e2 in [1 .. DigraphNrEdges(S!.DomainDigraph)] do
      if DigraphEdges(S!.DomainDigraph)[e2] = 
         [out2vertexmap[DigraphEdges(out1!.DomainDigraph)[e1][1]], 
          out2vertexmap[DigraphEdges(out1!.DomainDigraph)[e1][2]]] then
        if out1!.EdgeMap[e1] = S!.EdgeMap[e2] then
           Add(out2edgemap, [e2]);
           break;
        fi;
      fi;
    od;
  od;
  return [out1, WalkHomomorphism(out1!.DomainDigraph, S!.DomainDigraph, out2vertexmap, out2edgemap) * f2];
end);


InstallMethod(MaxFutureConeDepth, "for a walk homomorphism",
[IsWalkHomomorphism],
function(H)
  local verteximages;
  verteximages := List(DigraphVertices(H!.DomainDigraph), x-> ImageAsUnionOfCones(H, x));
  if ForAny(verteximages, x-> x= fail) then
    return fail;
  fi;

  return Maximum(Concatenation(List(Concatenation(verteximages), x-> Size(x[1])), [-1]));
end);

InstallMethod(MaxHistoryConeDepth, "for a walk homomorphism",
[IsWalkHomomorphism],
function(H)
  return MaxFutureConeDepth(DualWalkHomomorphism(H));
end);



InstallMethod(ImageAsUnionOfCones, "for a walk homomorphism and a domain vertex",
[IsWalkHomomorphism, IsInt],
function(H, v)
  local n, FullImageAutomatonStates, Verts, Outs, M, state, targetvertex,
        vertex, A2, A, D, badvertices, goodverts, tempbadvertices,
        goodedges, removebadedges, transmat, start, alphabet, out,
        w, whatAshouldbe, coneprefixes, wordtoendstate, i, j, newtransmat,
        wordtoconeautomaton, ending, goodedgesalph, prefixmachine;
  
  if not v in DigraphVertices(H!.DomainDigraph) then
    return fail;
  fi;
  
  targetvertex := H!.VertexMap[v];
  A := MinimalAutomaton(WalkHomomorphismVertexImageAutomaton(H, v));  
  D := H!.CoDomainDigraph;
  badvertices := [];
  tempbadvertices := Filtered([1.. DigraphNrVertices(D)], x-> ForAll(OutNeighbours(D)[x], y->y in badvertices));
  while Size(badvertices) < Size(tempbadvertices) do
    badvertices := tempbadvertices;
    tempbadvertices := Filtered([1.. DigraphNrVertices(D)], x-> ForAll(OutNeighbours(D)[x], y->y in badvertices));
  od;
  goodedges := Filtered([1 .. DigraphNrEdges(D)], x-> not DigraphEdges(D)[x][2] in badvertices);
  goodedgesalph := List(goodedges, x-> String(x)[1]);
  removebadedges := function(B)
     return MinimalAutomaton(Automaton("det", 
               NumberStatesOfAutomaton(B), 
               goodedges,
               List(goodedges, y->TransitionMatrixOfAutomaton(B)[y]),
               InitialStatesOfAutomaton(B),
               FinalStatesOfAutomaton(B)));
  end;
  A := removebadedges(A);

 
  n := NumberStatesOfAutomaton(A);
  goodverts := Filtered([1 .. DigraphNrVertices(D)], x-> not x in badvertices);
  H := IdentityWalkHomomorphism(D);
  Outs := List(DigraphVertices(D), x -> WalkHomomorphismVertexImageAutomaton(H, x));
  Apply(Outs, x->MinimalAutomaton(x));
  Apply(Outs, removebadedges);

  FullImageAutomatonStates := List([1 .. n], x->[]);
  for state in [1 .. n] do
    A2:= CopyAutomaton(A);
    SetInitialStatesOfAutomaton(A2, state);
    M := MinimalAutomaton(A2);
    for vertex in [1 .. DigraphNrVertices(D)] do
      if M = Outs[vertex] then
        AddSet(FullImageAutomatonStates[state], vertex);
      fi;
    od;
  od;


  alphabet := [1 .. AlphabetOfAutomaton(A)];
  transmat := TransitionMatrixOfAutomaton(A);
  start := InitialStatesOfAutomaton(A);
  ending := Filtered([1 .. NumberStatesOfAutomaton(A)], x-> not FullImageAutomatonStates[x] = []);

  newtransmat := List(transmat, x-> ShallowCopy(x));
  for i in [1 .. Size(newtransmat)] do
    for j in ending do;
      Unbind(newtransmat[i][j]);
    od;
  od;

  prefixmachine := Automaton("det", n, alphabet, newtransmat, start, ending);
  if IsEmptyLang(prefixmachine) then
    coneprefixes := [];
  elif AlphabetOfAutomaton(prefixmachine) = 0 then
    coneprefixes := [[]];
  else
    coneprefixes := FiniteRegularLanguageToListOfWords(prefixmachine);
  fi;

  wordtoendstate := function(w)
    if w = [] then
      return FullImageAutomatonStates[start[1]][1];
    fi;
    return DigraphEdges(D)[w[Size(w)]][2];
  end;

  wordtoconeautomaton := function(w)
    local properprefixes, prefixA, wA, P, newtrans;
    properprefixes := List([0 .. Size(w) - 1], x-> List([1 .. x], y-> w[y]));
    prefixA := ListOfWordsToAutomaton(goodedges, properprefixes);
    wA := ListOfWordsToAutomaton(goodedges, [w]);
    P := ProductOfLanguages(wA, Outs[wordtoendstate(w)]);
    newtrans := TransitionMatrixOfAutomaton(P);
    #the fact the the next few lines are neccasary suggests that there is a
    #problem with the automata package
    if AlphabetOfAutomaton(wA) = 0 and AlphabetOfAutomaton(P) = 1 then
      newtrans := [];
    fi;
    P := Automaton("det", NumberStatesOfAutomaton(P), goodedges, 
                          newtrans, 
                          InitialStatesOfAutomaton(P), 
                          FinalStatesOfAutomaton(P));
    return MinimalAutomaton(UnionAutomata(prefixA, P));
  end;

  whatAshouldbe := Automaton("det", 1, goodedges, List(goodedges, x-> [1]), [1], []);
  
  for w in coneprefixes do
    whatAshouldbe := MinimalAutomaton(UnionAutomata(whatAshouldbe, wordtoconeautomaton(w)));
  od;

  if not A = MinimalAutomaton(whatAshouldbe) then
    return fail;
  fi;

  out := List(coneprefixes, x-> [List(x, y->y), wordtoendstate(w)]);
  for w in out do
    if w[1] = [] then
      w[2] := targetvertex;
    fi;
  od;
  return out;
end);


InstallMethod(DualWalkHomomorphism,
"for a walk homomorphism",
[IsWalkHomomorphism],
function(H)
  local i, j, edges1, edges2, D1, D2, edgeperm1, edgeperm2, newedgemap;

  D1 := H!.DomainDigraph;
  D2 := H!.CoDomainDigraph;
  edges1 := List([1 .. DigraphNrEdges(D1)], x->[ShallowCopy(DigraphEdges(D1)[x]), x]);
  edges2 := List([1 .. DigraphNrEdges(D2)], x->[ShallowCopy(DigraphEdges(D2)[x]), x]);
  for i in [1 .. DigraphNrEdges(D1)] do
    edges1[i][1] := [edges1[i][1][2], edges1[i][1][1]];
  od;
  for i in [1 .. DigraphNrEdges(D2)] do
    edges2[i][1] := [edges2[i][1][2], edges2[i][1][1]];
  od;
  Sort(edges1);
  Sort(edges2);
  edgeperm1 := List(edges1, x-> x[2]);
  edgeperm2 := List(edges2, x-> x[2]);

  newedgemap := List(H!.EdgeMap, x->List([1 .. Size(x)], y->x[Size(x) + 1 - y]));
  newedgemap := List(edgeperm1, x-> newedgemap[x]);
  for i in [1 .. DigraphNrEdges(D1)] do
     for j in [1 .. Size(newedgemap[i])] do
       newedgemap[i][j] := Position(edgeperm2, newedgemap[i][j]);
     od;
  od;

  edges1 := [List(edges1, x->x[1][1]), List(edges1, x->x[1][2])];
  edges2 := [List(edges2, x->x[1][1]), List(edges2, x->x[1][2])];

  return WalkHomomorphism(Digraph(DigraphNrVertices(D1), edges1[1], edges1[2]),
                          Digraph(DigraphNrVertices(D2), edges2[1], edges2[2]),
                          H!.VertexMap,
                          newedgemap);
end);


InstallMethod(WalksOfGivenLength, "for a digraph and a non-negative integer",
[IsDigraph, IsInt],
function(D, n)
  local i, j, potentialpaths, paths;
  if n = 0 then
    return [[]];
  fi;
  potentialpaths := Tuples([1 .. DigraphNrEdges(D)], n);
  paths := [];
  for i in potentialpaths do
    if ForAll([1 .. Size(i) - 1], x->DigraphEdges(D)[i[x]][2] =DigraphEdges(D)[i[x+ 1]][1]) then
      Add(paths, ShallowCopy(i));
    fi;
  od;
  return paths;
end);


InstallMethod(LineDigraphWalkHomomorphism, "for a digraph and two integers",
[IsDigraph, IsInt, IsInt],
function(D, past, future)
  local vertices, edges, nrverts, edgepairs, leftvertices, rightvertices, 
        inductivestep, D2;

  if not past >= 0 and future >= 0 then
    return fail;
  fi;

  if past = 0 and future = 0 then
    return TrimWalkHomomorphism(IdentityWalkHomomorphism(D));
  fi;

  if past + future = 1 then
    vertices := WalksOfGivenLength(D, 1);
    Sort(vertices);
    edges := WalksOfGivenLength(D, 2);
    nrverts := Size(vertices);
    edgepairs := List(edges, x-> [Position(vertices, [x[1]]), Position(vertices, [x[2]])]);
    Sort(edgepairs);
    leftvertices := List(edgepairs, x->x[1]);
    rightvertices := List(edgepairs, x->x[2]);
    D2 := Digraph(nrverts, leftvertices, rightvertices);
    if future = 1 then
      return WalkHomomorphism(D2, D, List(vertices, 
                                          x->DigraphEdges(D)[x[1]][1]), 
                                     List(leftvertices, x-> [x]));
    fi;
    if past = 1 then
      return WalkHomomorphism(D2, D, List(vertices, 
                                          x->DigraphEdges(D)[x[1]][2]), 
                                     List(rightvertices, x-> [x]));
    fi;
  fi;
  if future > 0 then
    inductivestep := LineDigraphWalkHomomorphism(D, past, future - 1);
    return ComposeWalkHomomorphisms(LineDigraphWalkHomomorphism(inductivestep!.DomainDigraph, 0, 1), 
                                    inductivestep);
  fi;
  inductivestep := LineDigraphWalkHomomorphism(D, past - 1, future);
  return ComposeWalkHomomorphisms(LineDigraphWalkHomomorphism(inductivestep!.DomainDigraph, 1, 0), 
                                    inductivestep);
end);


InstallMethod(TrimWalkHomomorphism, "for a walk homomorphism",
[IsWalkHomomorphism],
function(H)
  local D, badvertices, edges, goodvertices, D2;
  D := H!.DomainDigraph;
  edges := List([1 .. DigraphNrEdges(D)], x-> [DigraphEdges(D)[x], x]);
  goodvertices := Filtered([1 .. DigraphNrVertices(D)], x-> OutNeighbours(D)[x] <> [] and InNeighbours(D)[x] <> []);
  if Size(goodvertices) = DigraphNrVertices(D) then
    return H;
  fi;
  D2 := InducedSubdigraph(D, goodvertices);
  edges := Filtered(edges, x-> x[1][1] in goodvertices and x[1][2] in goodvertices);
  edges := List(edges, x-> x[2]);
  return TrimWalkHomomorphism(WalkHomomorphism(D2, H!.CoDomainDigraph, 
                          List([1 .. DigraphNrVertices(D2)], x -> H!.VertexMap[goodvertices[x]]),
                          List([1 .. DigraphNrEdges(D2)], x -> H!.EdgeMap[edges[x]])));  
  return true;
end);


InstallMethod(\*, "for a pair of walk homomorphisms",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(f, g)
  return ComposeWalkHomomorphisms(f, g);
end);


#note that this equality cares about the order of the edges in a digraph and this
#not neccassarily shared by digraphs equal under =
InstallMethod(\=, "for a pair of walk homomorphisms",
[IsWalkHomomorphism, IsWalkHomomorphism],
function(f, g)
  return DigraphVertices(f!.DomainDigraph) = DigraphVertices(g!.DomainDigraph) and
         DigraphEdges(f!.DomainDigraph) = DigraphEdges(g!.DomainDigraph) and
         DigraphVertices(f!.CoDomainDigraph) = DigraphVertices(g!.CoDomainDigraph) and
         DigraphEdges(f!.CoDomainDigraph) = DigraphEdges(g!.CoDomainDigraph) and
         f!.VertexMap = g!.VertexMap and f!.EdgeMap = g!.EdgeMap;

end);

#This function "removes incomplete responce" meaning that it trims the folding and replaces the 
#vertex of each domain vertex with the vertex at the end of the longest common
# prefix of the image of the vertex
# it the replaces the walk on each edge with the difference between (the longest
# common prefix of the image of it's start vertex) and (the longest common
# prefix of the infinite walks in the image of the edge).

#This is useful because of the following claims
#Claim1: removing incomplete responce from a trimmed UDAF folding doesn't change the induced
#UDAF isomorphism
#'Proof': Using the fact that the target is an UDAF digraph it follows that the new 
#image of each infinite forward walk is the same as before with the longest common prefix 
#of the image of it's start vertex removed. In particular the new walk homomorphism is not 
#degerate
#By viewing a biinfinite route as a limit of forwards infinite routes the result follows
#
#Claim2: If f_1 f_2:D1 to D2 are trimmed UDAF Foldings with complete responce which induce the
#same UDAF ISomorphism then f_1 = f_2
#'Proof': The UDAF isomorphism induces a map from the irrational backwards walks of D1
#to the irrational backwards walks of D2. Thus we can deduce exactly which walk each edge
#must map to
InstallMethod(RemoveIncompleteResponse, "for a pair of walk homomorphisms",
[IsWalkHomomorphism],
function(input)
  local edges, flag, H, prefixes, v, v2, edgetowalk, pair;

  if not IsUDAFDigraph(input!.CoDomainDigraph) then
   return fail;
  fi;
  H := TrimWalkHomomorphism(input);
  prefixes := List(DigraphVertices(H!.DomainDigraph), x->ImageAsUnionOfCones(H, x));
  
  #we need to write each cone as a cone whose associated vertex has
  #at least two edges coming out of it
  flag := true;
  while flag do
    flag := false;
    for v in DigraphVertices(H!.DomainDigraph) do
      for pair in [1 .. Size(prefixes[v])] do
        edges := Filtered([1 .. DigraphNrEdges(H!.DomainDigraph)], 
                       x-> DigraphEdges(H!.DomainDigraph)[x][1] =prefixes[v][pair][2]);
        if Size(edges) = 1 then
          flag := true;
          Append(prefixes[v][pair][1], H!.EdgeMap[edges[1]]);
          prefixes[v][pair][2] := DigraphEdges(H!.DomainDigraph)[edges[1]][2];
        fi;
      od;
    od;
  od;

  for v in DigraphVertices(H!.DomainDigraph) do
    Apply(prefixes[v], x->x[1]);
    prefixes[v] := GreatestCommonPrefix(prefixes[v]); 
    v2 := H!.VertexMap[v];
    if not prefixes[v] = [] then
      v2 := DigraphEdges(H!.CoDomainDigraph)[prefixes[v][Size(prefixes[v])]][2];
    fi;
    prefixes[v] := [prefixes[v], v2]; 
  od;

  edgetowalk := function(e)
    local vertexprefix1, vertexprefix2, edgeprefix;
    vertexprefix1 := prefixes[DigraphEdges(H!.DomainDigraph)[e][1]][1];
    vertexprefix2 := prefixes[DigraphEdges(H!.DomainDigraph)[e][2]][1];
    edgeprefix := Concatenation(H!.EdgeMap[e], vertexprefix2);
    return Minus(edgeprefix, vertexprefix1);
  end;

  return WalkHomomorphism(H!.DomainDigraph, H!.CoDomainDigraph,
                          List(prefixes, x-> x[2]),
                          List([1 .. DigraphNrEdges(H!.DomainDigraph)], 
                                                           edgetowalk));

end);


InstallMethod(IsSynchronousWalkHomomorphism, "for a walk homomorphism",
[IsWalkHomomorphism],
function(H)
  return ForAll(H!.EdgeMap, x-> Size(x) = 1);
end);


InstallMethod(SynchronousRemoveIncompleteResponse, "for a synchronous walk homorphism",
[IsWalkHomomorphism],
function(input)
  local D, v, edges, H, e, nextvertex, n, edgemap;
  H := TrimWalkHomomorphism(input);
  if not IsSynchronousWalkHomomorphism(H) then
    return fail;
  fi;

  D := H!.DomainDigraph;
  edges := [];
  e := 1;
  for v in DigraphVertices(D) do
    Add(edges, []);
    for n in OutNeighbours(D)[v] do
      Add(edges[Size(edges)], H!.EdgeMap[e]);
      e := e+1;
    od;
    edges[Size(edges)] := Set(edges[Size(edges)]);
    if not Size(edges[Size(edges)]) = 1 then
      return H;
    fi;
    edges[Size(edges)] := edges[Size(edges)][1];
  od;

  nextvertex := List([1 .. DigraphNrVertices(D)], x-> OutNeighbours(D)[x][1]);
  edgemap := List([1 .. DigraphNrEdges(D)], x->edges[DigraphEdges(D)[x][2]]);

  return SynchronousRemoveIncompleteResponse(WalkHomomorphism(D, 
                          H!.CoDomainDigraph, 
                          List(nextvertex, x->H!.VertexMap[x]),
                          edgemap));
  
end);

