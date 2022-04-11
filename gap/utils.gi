#############################################################################
##
#W  utils.gi
#Y  Copyright (C) 2022                               Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains utility functions.

InstallGlobalFunction(AutshiftMakeDoc,
function()
  MakeGAPDocDoc(Concatenation(PackageInfo("autshift")[1]!.InstallationPath, "/doc"),
                "main.xml", ["../PackageInfo.g",
                             "title.xml",
                             "transducer.xml",
                             "shiftoperations.xml",
                             "utils.xml"], "autshift", "MathJax", "../../..");
  return;
end);

InstallMethod(Draw, 
"for an UDAF transducer",
[IsUDAFIsomorphism],
function(T)
  Draw(T!.MinimalUDAFTransducer);
end);



InstallMethod(Draw, 
"for an UDAF transducer",
[IsUDAFTransducer],
function(T)
  Splash(DotUDAFTransducer(T));
end);


InstallMethod(DrawSynchronous, 
"for a shift isomorphism",
[IsShiftIsomorphism],
function(T)
  Draw(T!.SynchronousUDAFTransducer);
end);


InstallMethod(Draw, 
"for a walk homomorphism",
[IsWalkHomomorphism],
function(T)
  Splash(DotWalkHomomorphism(T));
end);


InstallMethod(Draw, 
"for an UDAF transducer",
[IsShiftIsomorphism],
function(T)
  Splash(DotUDAFTransducerWithVertexLabels(T!.MinimalUDAFTransducer, 
               List(T!.Annotation, x-> String(x))));
end);



InstallMethod(DotUDAFTransducer, 
"for an UDAF transducer",
[IsUDAFTransducer],
function(T)
  return DotUDAFTransducerWithVertexLabels(T, 
           List(DigraphVertices(T!.Digraph), x-> String(x)));
end);


InstallMethod(DotUDAFTransducerWithVertexLabels, 
"for an UDAF tranducer and a list of vertex labels",
[IsUDAFTransducer, IsDenseList],
function(T, L)
  local H1, H2, i, j, label, m, n, st, str, verts, D, e, D2, verts2,
        m2, label2, D1, verts1, label1, m1;

  H1 := T!.DomainFolding;
  H2 := T!.CoDomainFolding;

#TODO check that L is valid

  D := T!.Digraph;
  verts := DigraphVertices(D);
  m     := DigraphNrVertices(D);
  str   := "//dot\n";
  label := L;

  D1 := T!.DomainDigraph;
  verts1 := DigraphVertices(D1);
  m1     := DigraphNrVertices(D1);
  label1 := List(verts1, x -> Concatenation("D", String(x)));


  D2 := T!.CoDomainDigraph;
  verts2 := DigraphVertices(D2);
  m2     := DigraphNrVertices(D2);
  label2 := List(verts2, x -> Concatenation("C", String(x)));


  Append(str, "digraph finite_state_machine{\n");
  Append(str, "rankdir=LR;\n");
  Append(str, "node [shape=circle]\n");
 
  for i in verts do
    Append(str, Concatenation(label[i], "\n"));
  od;
  for i in verts1 do
    Append(str, Concatenation(label[i], "\n"));
  od;
 
 for i in verts2 do
    Append(str, Concatenation(label2[i], "\n"));
  od;

  e := 1;
  for i in verts do
    n := 0;
    for j in DigraphOutEdges(D, i) do
      n := n + 1;
      st := "";
      Append(st, String(H1!.EdgeMap[e]));
      Append(st, "/");
      Append(st, String(H2!.EdgeMap[e]));
      Append(str, Concatenation(label[i], " -> ", label[j[2]]));
      Append(str, Concatenation(" [label=\"", st, "\"]"));
      Append(str, "\n");
      e := e + 1;
    od;
  od;

  e := 1;
  for i in verts1 do
    n := 0;
    for j in DigraphOutEdges(D1, i) do
      n := n + 1;
      st := "";
      Append(st, String(e));
      Append(str, Concatenation(label1[i], " -> ", label1[j[2]]));
      Append(str, Concatenation(" [label=\"", st, "\"]"));
      Append(str, "\n");
      e := e + 1;
    od;
  od;

  e := 1;
  for i in verts2 do
    n := 0;
    for j in DigraphOutEdges(D2, i) do
      n := n + 1;
      st := "";
      Append(st, String(e));
      Append(str, Concatenation(label2[i], " -> ", label2[j[2]]));
      Append(str, Concatenation(" [label=\"", st, "\"]"));
      Append(str, "\n");
      e := e + 1;
    od;
  od;

  Append(str, "}\n");
  return str;
end);



InstallMethod(DotWalkHomomorphism, "for a walkhomomorphism",
[IsWalkHomomorphism],
function(H)
  local i, j, label, m, n, st, str, verts, D, e, D2, verts2, m2, label2;

  D := H!.DomainDigraph;
  verts := DigraphVertices(D);
  m     := DigraphNrVertices(D);
  str   := "//dot\n";
  label := List(verts, x -> Concatenation("D", String(x)));

  D2 := H!.CoDomainDigraph;
  verts2 := DigraphVertices(D2);
  m2     := DigraphNrVertices(D2);
  label2 := List(verts2, x -> Concatenation("C", String(x)));


  Append(str, "digraph finite_state_machine{\n");
  Append(str, "rankdir=LR;\n");
  Append(str, "node [shape=circle]\n");

  for i in verts do
    Append(str, Concatenation(label[i], "\n"));
  od;
  for i in verts2 do
    Append(str, Concatenation(label2[i], "\n"));
  od;

  e := 1;
  for i in verts do
    n := 0;
    for j in DigraphOutEdges(D, i) do
      n := n + 1;
      st := "";
      Append(st, String(H!.EdgeMap[e]));
      Append(str, Concatenation(label[i], " -> ", label[j[2]]));
      Append(str, Concatenation(" [label=\"", st, "\"]"));
      Append(str, "\n");
      e := e + 1;
    od;
  od;

  e := 1;
  for i in verts2 do
    n := 0;
    for j in DigraphOutEdges(D2, i) do
      n := n + 1;
      st := "";
      Append(st, String(e));
      Append(str, Concatenation(label2[i], " -> ", label2[j[2]]));
      Append(str, Concatenation(" [label=\"", st, "\"]"));
      Append(str, "\n");
      e := e + 1;
    od;
  od;


  Append(str, "}\n");
  return str;
end);

