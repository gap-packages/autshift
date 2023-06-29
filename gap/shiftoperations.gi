#############################################################################
##
#W  shiftoperations.gi
#Y  Copyright (C) 2022                               Luke Elliott
##
##  Licensing information can be found in the README file of this package.
##
#############################################################################
##

# This file contains methods for operations that relate to automorphisms of the shift.


#InstallMethod(ActionOnNecklaces, "for a positive integer and a transducer",
#[IsPosInt, IsTransducerOrRTransducer],
#function(necklacesize, T)
#  local X, x, i, j, permutation;
#  if not IsSynchronizingTransducer(T) then
#    return fail;
#  fi;
#  X := PrimeNecklaces(NrInputSymbols(T), necklacesize);
#  permutation := [];
#  for i in [1 .. Size(X)] do
#    x := X[i]^T;
#    for j in [1 .. Size(X)] do
#      if ShiftEquivalent(X[j], x) then
#        Add(permutation, j);
#      fi;
#    od;
#    if not Size(permutation) = i then
#      return fail;
#    fi;
#  od;
#  return PermList(permutation);
#end);

#InstallMethod(InOn, "for a Transducer",
#[IsTransducer],
#function(T)
#  if IsCompletableCore(T) then
#    return IsSynchronizingTransducer(CoreCompletion(T)^-1);
#  else
#    return false;
#  fi;
#end);

#InstallMethod(InLn, "for a Transducer",
#[IsTransducer], x -> InOn(x) and IsLipschitzTransducer(x));

#InstallMethod(CanonicalAnnotation, "for a Transducer",
#[IsTransducer],
#function(T)
#  local slen, tout, zerostate, word, annotation;
#  if not InLn(T) then
#    return fail;
#  fi;
#  slen := TransducerSynchronizingLength(T);
#  zerostate := TransducerFunction(T, ListWithIdenticalEntries(slen, 0), 1)[2];
#  annotation := [];
#  annotation[zerostate] := 0;
#  for word in Tuples(InputAlphabet(T), slen) do
#    tout := TransducerFunction(T, word, 1);
#    annotation[tout[2]] := Size(tout[1]) - slen;
#  od;
#  return annotation;
#end);

#InstallMethod(LnBlockCodeTransducer, "for a transducer", [IsTransducer],
#function(T)
#  local ann, slen, wlen, f, maxdiff, i, j;
#  if not InLn(T) then
#    return fail;
#  fi;
#  ann := CanonicalAnnotation(T);
#  slen := TransducerSynchronizingLength(T);
#  wlen := slen + Maximum(ann) - Minimum(ann);
#  f := function(word)
#    local writtenword, sstate;
#    sstate := TransducerFunction(T, word{[1 .. slen]}, 1)[2];
#    writtenword := TransducerFunction(T, word{[slen + 1 .. wlen + 1]}, sstate)[1];
#    return [writtenword[Maximum(ann) - ann[sstate] + 1]];
#  end;
#  return BlockCodeTransducer(NrInputSymbols(T), wlen, f);
#end);

#InstallMethod(MinSyncSync, "for a Transducer",
#[IsTransducer],
#function(T)
#   local out, lambda, i, state, letter;
#   if not IsCoreTransducer(T) and IsSynchronousTransducer(T) then
#     return fail;
#   fi;
#   out := CombineEquivalentStates(T);
#   for i in OutputFunction(out) do
#     if Size(Set(i)) > 1 then
#       return out;
#     fi;
#   od;
#   lambda := [];
#   for state in States(out) do
#     Add(lambda, []);
#     for letter in InputAlphabet(T) do
#       Add(lambda[state],
#           OutputFunction(out)[TransitionFunction(out)[state][letter + 1]][1]);
#     od;
#   od;
#   return MinSyncSync(Transducer(NrInputSymbols(out), NrOutputSymbols(out),
#                                 TransitionFunction(out), lambda));
#end);

#InstallMethod(OnInverse, "for a Transducer",
#[IsTransducer],
#function(T)
#  if not InOn(T) then
#    return fail;
#  fi;
#  return TransducerCore(MinimalTransducer(CoreCompletion(T)^-1));
#end);

#InstallMethod(\+, "for a Transducer", [IsTransducer, IsTransducer],
#function(T1,T2)
#  if not InOn(T1) and InOn(T2) then
#    return fail;
#  fi;
#  return CoreProduct(T1, T2);
#end);

#InstallMethod(\-, "for a pair of Transducers", [IsTransducer, IsTransducer],
#function(T1, T2)
#  if not InOn(T1) and InOn(T2) then
#    return fail;
#  fi;
#  return T1 + OnInverse(T2);
#end);

#InstallMethod(\*, "for an integer and a transducer", [IsInt, IsTransducer],
#function(n, T)
#  local k, t;
#  if not InOn(T) then
#    return fail;
#  fi;
#  if n = 0 then
#    return IdentityTransducer(NrInputSymbols(T));
#  fi;
#  t := CopyTransducerWithInitialState(T, 1);
#  if n < 0 then
#    n := -n;
#    t := OnInverse(t);
#  fi;
#  k := 1;
#  while 2*k <= n do
#    k := 2*k;
#    t := t+t;
#  od;
#  return t + (n-k)*T;
#end);

#InstallMethod(ASProd, "for a pair of transducers",
#[IsTransducer, IsTransducer],
#function(T1, T2)
#  if not (IsSynchronousTransducer(T1) and IsCoreTransducer(T1) and
#          IsSynchronousTransducer(T2) and IsCoreTransducer(T2)) then
#    return fail;
#  fi;
#  return TransducerCore(CombineEquivalentStates(RemoveInaccessibleStates(T1*T2)));
#end);

#InstallMethod(Onlessthan, "for a pair of transducers",
#[IsTransducer, IsTransducer],
#function(T1,T2)
#  local M1, M2, S1, S2, q1, q2, word, w1, w2, i;
#  if not (InOn(T1) and InOn(T2)) then
#    return fail;
#  fi;
#  if NrInputSymbols(T1) < NrInputSymbols(T2) then
#    return true;
#  elif NrInputSymbols(T2) < NrInputSymbols(T1) then
#    return false;
#  fi;
#  M1 := TransducerCore(MinimalTransducer(T1));
#  M2 := TransducerCore(MinimalTransducer(T2));
#  if IsomorphicTransducers(M1, M2) then
#    return false;
#  fi;
#  S1 := TransducerSynchronizingLength(M1);
#  S2 := TransducerSynchronizingLength(M2);
#  if S1 < S2 then
#    return true;
#  elif S2 < S1 then
#    return false;
#  fi;
#  if NrStates(M1) < NrStates(M2) then
#    return true;
#  elif NrStates(M2) < NrStates(M1) then
#    return false;
#  fi;
#  q1 := TransducerFunction(M1, List([1 .. S1], x -> 0), 1)[2];
#  q2 := TransducerFunction(M2, List([1 .. S2], x -> 0), 1)[2];
#  word := [0];
#  while true do
#    w1 := TransducerFunction(M1, word, q1)[1];
#    w2 := TransducerFunction(M2, word, q2)[1];
#    if Size(w1) < Size(w2) then
#      return true;
#    elif Size(w2) < Size(w1) then
#      return false;
#    fi;
#    if w1 < w2 then
#      return true;
#    elif w2 < w1 then
#      return false;
#    fi;
#    for i in [0 .. Size(word) - 1] do
#      if word[Size(word) - i] <> NrInputSymbols(T1) - 1 then
#        word[Size(word) - i] := word[Size(word) - i] + 1;
#        break;
#      else
#        word[Size(word) - i] := 0;
#      fi;
#    od;
#    if i = Size(word) - 1 and word[1] = 0 then
#      Add(word, 0);
#    fi;
#  od;
#end);

#InstallMethod(LnToLnk, "for a transducer and a positive integer",
#[IsTransducer, IsPosInt],
#function(T, k)
#  local blockcode, NewToOld, OldToNew, f, SLen;
#  if not InLn(T) then
#    return fail;
#  fi;
#  if k = 1 then
#    return T;
#  fi;
#  NewToOld := function(n)
#   return List([0 .. (k - 1)], x -> Int(RemInt(n, NrInputSymbols(T) ^ (x + 1))
#                                                / (NrInputSymbols(T) ^ x)));
#  end;
#  OldToNew := function(l)
#    return Sum(List([0 .. Size(l) - 1],
#               y -> l[y + 1] * (NrInputSymbols(T) ^ y)));
#  end;
#  if not IsSynchronousTransducer(T) then
#    blockcode := CombineEquivalentStates(LnBlockCodeTransducer(T));
#  else
#    blockcode := T;
#  fi;
#
#  SLen := TransducerSynchronizingLength(blockcode);
#  f := function(word)
#    local oldin, oldout;
#    oldin := Concatenation(List(word, x -> NewToOld(x)));
#    oldout := TransducerFunction(blockcode,
#                                 oldin{[Size(oldin) - k + 1 .. Size(oldin)]},
#                                 TransducerFunction(blockcode,
#                                       oldin{[1 .. Size(oldin) - k]}, 1)[2])[1];
#    return [OldToNew(oldout)];
#  end;
#  return BlockCodeTransducer(NrInputSymbols(T)^k, Int(SLen/2) + 2, f);
#end);

#InstallMethod(OnOrder, "for a transducer",
#[IsTransducer],
#function(T)
#  local T1, power;
#  T1 := T;
#  power := 1;
#  if NrInputSymbols(T) =2 and Order(ActionOnNecklaces(10, T)) > 100 then
#    return infinity;
#  fi;
#  while not IsomorphicTransducers(0*T, T1) do
#    T1 := T1 + T;
#    power := power + 1;
#    if NrStates(T1) > 100 then
#      return infinity;
#    fi;
#  od;
#  return power;
#end);

#InstallMethod(StateSynchronizingWords, "for a transducer",
#[IsTransducer],
#function(T)
#  local outputs, tuple;
#  if not IsCoreTransducer(T) then
#    return fail;
#  fi;
#  outputs := List(States(T), x-> []);
#
#  for tuple in Tuples(InputAlphabet(T), TransducerSynchronizingLength(T)) do
#    Add(outputs[TransducerFunction(T, tuple, 1)[2]], tuple);
#  od;
#
#  return outputs;
#end);

#InstallMethod(SynchronousLn, "for a transducer",
#[IsTransducer],
#function(T)
#  if not InLn(T) then
#    return fail;
#  fi;
#  if IsSynchronousTransducer(T) then
#    return MinSyncSync(T);
#  fi;
#  return MinSyncSync(LnBlockCodeTransducer(T));
#end);

#InstallMethod(HomeomorphismStates, "for a transducer",
#[IsTransducer],
#function(T)
#  local state, output;
#  output := [];
#  for state in States(T) do
#    if IsBijectiveTransducer(CopyTransducerWithInitialState(T, state)) then
#      Add(output, state);
#    fi;
#  od;
#  return output;
#end);


#InstallMethod(GyrationValues, "for a transducer",
#[IsTransducer, IsDenseList],
#function(T, x)
#  local i_x, r_x, currentword;
#  if not (IsSynchronousTransducer(T) and InOn(T)) then
#	ErrorNoReturn("aaa: GyrationValues: usage,\n",
#                  "the given transducer must be an element",
#                  "of autshift,");
#  fi;
#  if ForAny(x, y -> not y in [0 .. NrInputSymbols(T)]) then
#    ErrorNoReturn("aaa: GyrationValues: usage,\n",
#                  "the given word must be in the alphabet",
#                  "of the given transducer,");
#  fi;
#  i_x:=1;
#  currentword := x^T;
#  while not ShiftEquivalent(x, currentword) do
#    i_x := i_x + 1;
#    currentword := currentword^T;
#  od;
#  r_x := 0;
#  while x <> Concatenation(currentword{[r_x +1 .. Size(x)]}, currentword{[1 .. r_x]}) do
#    r_x := r_x + 1;
#  od;
#  return [i_x, r_x];
#end);

#InstallMethod(GyrationAtLevel, "for a transducer",
#[IsTransducer, IsPosInt],
#function(T, l)
#  local gyr_l;
#  if not (IsSynchronousTransducer(T) and InOn(T)) then
#        ErrorNoReturn("aaa: GyrationAtLevel: usage,\n",
#                  "the given transducer must be an element",
#                  "of autshift,");
#  fi;
#  gyr_l := Sum(List(PrimeNecklaces(NrInputSymbols(T), l), x -> GyrationValues(T, x)[2]));
#  while gyr_l >= l do
#    gyr_l := gyr_l - l;
#  od;
#  return gyr_l;
#end);

#InstallMethod(LnShayoHomomorphism, "for a transducer",
#[IsTransducer],
#function(T)
#  local n, img;
#  if not InLn(T) then
#    return fail;
#  fi;
#  n := NrInputSymbols(T);
#  img := Size(ImageAsUnionOfCones(T));
#  while IsInt(img/n) do
#    img := Int(img/n);
#  od;
#  return img;
#end);

#InstallMethod(OnShayoHomomorphism, "for a transducer",
#[IsTransducer],
#function(T)
#  if not InOn(T) then
#    return fail;
#  fi;
#  return RemInt(Size(ImageAsUnionOfCones(T)), NrInputSymbols(T));
#end);

#InstallMethod(OneSidedDecomposition, "for a transducer",
#[IsTransducer],
#function(T)
#  local T2, factor, makefactor, inverse, i, j, k, output, nrep, nrelated, Bstates, BTransitions, Boutputs, state;
#  if not InLn(T) then
#    return fail;
#  fi;
#  for i in States(T) do
#    if not Size(Set(OutputFunction(T)[i])) = NrInputSymbols(T) then
#      return fail;
#    fi;
#  od;
#
#  makefactor := function(T)
#    inverse := SynchronousLn(OnInverse(T));
#
#    for i in States(inverse) do
#      if not Size(Set(OutputFunction(inverse)[i])) = NrInputSymbols(inverse) then
#        return fail;
#      fi;
#    od;
#
#    for j in States(inverse) do
#      for k in [j + 1 .. NrStates(inverse)] do
#        if ForAll(InputAlphabet(inverse), x -> TransitionFunction(inverse)[j][Position(OutputFunction(inverse)[j], [x])] = TransitionFunction(inverse)[k][Position(OutputFunction(inverse)[k], [x])]) then
#          inverse := CopyTransducerWithInitialState(inverse, j);
#          inverse := CopyTransducerWithInitialState(inverse, k);
#        break;
#        fi;
#      od;
#    od;
#
#    nrelated:= function(q_1,q_2,n)
#      if n = 0 then
#        return q_1=q_2;
#      fi;
#      return ForAll([1 .. NrInputSymbols(inverse)], y -> nrelated(TransitionFunction(inverse)[q_1][y], TransitionFunction(inverse)[q_2][y], n-1));
#    end;
#  
#    nrep := function(q, n)
#      for j in States(inverse) do
#        if nrelated(j, q, n) then
#          return j;
#        fi;
#      od;
 #   end;
#
 #   for i in States(T) do
  #    if nrelated(1, 2, i) then
#        break;
 #     fi;
 #   od;
#
#    i:= i-1;
# 
#    Bstates := List(States(inverse), x -> nrep(x, i));
#    Bstates := Set(Bstates);
#    BTransitions := [];
#    for state in Bstates do
#      Add(BTransitions, List(TransitionFunction(inverse)[state], y-> nrep(y, i)));
#    od;
#  
#    Boutputs := List(Bstates, x-> List([0 .. NrInputSymbols(inverse)-1], y-> [y]));
#    for j in [0 .. NrInputSymbols(inverse)-1] do
#      Boutputs[2][Position(OutputFunction(inverse)[2], [j])] := [Position(OutputFunction(inverse)[1], [j]) - 1];
#    od;
#
#    for j in [1 .. Size(BTransitions)] do
#      for k in [1 .. Size(BTransitions[j])] do
#        BTransitions[j][k] := Position(Bstates, nrep(BTransitions[j][k], i));
#      od;
#    od;
#  
#    return Transducer(NrInputSymbols(inverse), NrOutputSymbols(inverse), BTransitions, Boutputs);
#  end;  
#  output := [];
#
#  T2:= CopyTransducerWithInitialState(T, 1);
#  while not NrStates(T2) = 1 do
#    factor := makefactor(T2);
#    Add(output, OnInverse(factor));
#    T2 := T2 + factor;
#  od;
#  if not IsomorphicTransducers(T2, IdentityTransducer(NrInputSymbols(T2))) then
#    Add(output, T2);
#  fi;
#  
#  return Reversed(output);
#end);
#
InstallMethod(IsLipschitzGNSTransducer, "for a transducer",
[IsGNSTransducer],
function(T)
  local s, statepath, letterpath, currentstate, nonconstantstates;
  nonconstantstates := States(T);
  SubtractSet(nonconstantstates, GNSTransducerConstantStateOutputs(T)[1]);
  for s in nonconstantstates do;
    statepath := [s];
    letterpath := [0];
    currentstate :=  GNSTransducerFunction(T, [0], s)[2];
    while statepath <> [] do
      if currentstate = s and
        Size(GNSTransducerFunction(T, letterpath, s)[1]) < Size(letterpath) then
        return false;
      fi;
      if not currentstate in statepath then
        Add(letterpath, 0);
        Add(statepath, currentstate);
      else
        letterpath[Size(letterpath)] := letterpath[Size(letterpath)] + 1;
        while letterpath <> [] and
              letterpath[Size(letterpath)] = NrInputSymbols(T) do;
          Remove(letterpath);
          Remove(statepath);
          if letterpath <> [] then
            letterpath[Size(letterpath)] := letterpath[Size(letterpath)] + 1;
          fi;
        od;
      fi;
      currentstate := GNSTransducerFunction(T, letterpath, s)[2];
    od;
  od;
  return true;
end);


InstallMethod(GNSTransducerCore, "for a transducer",
[IsGNSTransducer],
function(T)
  local SLen;
  SLen := GNSTransducerSynchronizingLength(T);
  if SLen = infinity then
    ErrorNoReturn("autshift: GNSTransducerCore: usage,\n",
                  "the transducer must be synchronizing ");
  fi;
  
  return RemoveInaccessibleStates(CopyGNSTransducerWithInitialState(T,
          GNSTransducerFunction(T, ListWithIdenticalEntries(SLen, 0), 1)[2]));
end);

InstallMethod(IsCoreGNSTransducer, "for a transducer",
[IsGNSTransducer], T -> IsSynchronizingGNSTransducer(T)
and NrStates(T) = NrStates(GNSTransducerCore(T)));


#InstallMethod(IsCompletableCore, "for a transducer",
#[IsTransducer],
#function(T)
#  return not IsDegenerateTransducer(T)
#         and IsInjectiveTransducer(T) and HasClopenImage(T)
#         and IsCoreTransducer(T);
#end);


# It appears that this function doesn't work
#InstallMethod(ImageAsUnionOfCones, "for a transducer",
#[IsTransducer],
#function(T)
#  local A, NrS, Pairs, Alph, Cones, GoodStates, TMat, Word, StatePath,
#        letter, pos, target;
#  if IsDegenerateTransducer(T) then
#    ErrorNoReturn("autshift: ImageAsUnionOfCones: usage,\n",
#                  "the given transducer must be nondegenerate ");
#  fi;
#  Alph := OutputAlphabet(T);
#  A  := MinimalAutomaton(TransducerImageAutomaton(T));
#  NrS  := NumberStatesOfAutomaton(A);
#  if NrS = 1 then
#    return [[]];
#  fi;
#  Cones := [];
#  GoodStates := FinalStatesOfAutomaton(A);
#  if not Size(GoodStates) = NrS - 1 then
#    return fail;
#  fi;
#  TMat := TransitionMatrixOfAutomaton(A);
#  Word := [];
#  StatePath := [InitialStatesOfAutomaton(A)[1]];
#  letter := 0;
#  repeat
#    target := TMat[letter + 1][StatePath[Size(StatePath)]];
#    pos := Position(StatePath, target);
#    if pos = fail then
#      Add(Word, letter);
#      Add(StatePath, target);
#      letter := 0;
#    elif pos = Size(StatePath) then
#      if Size(Set(List(TMat, x -> x[target]))) > 1 then
#        return fail;
#      fi;
#      if target in GoodStates then
#        Add(Cones, ShallowCopy(Word));
#      fi;
#      letter := Word[Size(Word)] + 1;
#      Remove(Word);
#      Remove(StatePath);
#    else
#      return fail;
#    fi;
#    while Word <> [] and letter = Size(Alph) do
#      letter := Word[Size(Word)] + 1;
#      Remove(Word);
#      Remove(StatePath);
#    od;
#  until letter = Size(Alph);
#  return Cones;
#end);


#InstallMethod(HasClopenImage, "for a Transducer",
#[IsTransducer],
#function(T)
# return ImageAsUnionOfCones(T) <> fail;
#end);


