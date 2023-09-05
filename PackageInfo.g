#
# autshift: implements algorithms for automorphisms of the shift and related 
# objects
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#

SetPackageInfo( rec(

PackageName := "autshift",
Subtitle := "Algorithms for Automotphisms of the Shift and related objects",
Version := "0.1.0",
Date := "12/07/2022", # dd/mm/yyyy format
License := "GPL-3.0-or-later",

AutoDoc := rec(
  TitlePage := rec(
    Abstract :=
      """
      The &autshift; package is a &GAP; package containing methods for
      transducers that can be used to represent isomorphisms between subshifts
      of finite type and related objects. It implements the some of processes
      described in the papers <URL>https://arxiv.org/abs/2004.08478</URL> and
      <URL>https://arxiv.org/abs/2112.13359</URL> and builds on the &GAP;
      package <URL Text="aaa">https://github.com/gap-packages/aaa</URL>.
      """,
    Acknowledgements :=
      """
      This package was created by Luke Elliott under the funding of ESPRC
      grant EP/R032866/1 and supervised by Collin Bleak. Special thanks to
      James Mitchell and Michael Torpey for their technical support.
      """,
    ),
),

Persons := [
  rec(
    IsAuthor := true,
    IsMaintainer := true,
    FirstNames := "Luke",
    LastName := "Elliott",
    WWWHome := "https://le27.github.io/Luke-Elliott/",
    PostalAddress := "Mathematical Institute, North Haugh, St Andrews, Fife, KY16 9SS, Scotland",
    Place := "St Andrews",
    Institution := "University of St Andrews",
  ),
rec(
      LastName      := "GAP Team",
      FirstNames    := "The",
      IsAuthor      := false,
      IsMaintainer  := true,
      Email         := "support@gap-system.org",
  )
],

SourceRepository := rec(
    Type := "git",
    URL := Concatenation( "https://github.com/gap-packages/", ~.PackageName ),
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := Concatenation( "https://gap-packages.github.io/", ~.PackageName ),
README_URL      := Concatenation( ~.PackageWWWHome, "/README.md" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "/PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL,
                                 "/releases/download/v", ~.Version,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

AbstractHTML :=  
      """
      The <span class="pkgname">autshift</span> package is a GAP package containing methods for
      transducers that can be used to represent isomorphisms between
      subshifts of finite type and related objects. It implements the
      some of processes described in the papers
      <a href="https://arxiv.org/abs/2004.08478">https://arxiv.org/abs/2004.08478</a> and
      <a href="https://arxiv.org/abs/2112.13359">https://arxiv.org/abs/2112.13359</a> and
      builds on the GAP package <a href="https://github.com/gap-packages/aaa">aaa</a>.
      """,

PackageDoc := rec(
  BookName  := ~.PackageName,
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := ~.Subtitle,
),

Dependencies := rec(
  GAP := ">=4.10",
  NeededOtherPackages := [["automata", ">=0.0.0"],
                          ["digraphs", ">=0.15.0"],
                          ["fr", ">=2.4.6"],
                          ["aaa", ">=0.0.0"],
                          ["datastructures", ">= 0.2.5"]],
  SuggestedOtherPackages := [],
  ExternalConditions := []),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));
