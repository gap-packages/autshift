#
# autshift: implements algorithms for automorphisms of the shift and related 
# objects
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#
##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY VERSION "0.1.0">
##  <!ENTITY GAPVERS "4.8.0">
##  <!ENTITY ARCHIVENAME "autshift-0.1.0">
##  <!ENTITY COPYRIGHTYEARS "2022">
##  <#/GAPDoc>


SetPackageInfo( rec(

PackageName := "autshift",
Subtitle := "Algorithms for Automotphisms of the Shift and related objects",
Version := "0.1.0",
Date := "12/07/2022", # dd/mm/yyyy format
License := "GPL-3.0-or-later",

AutoDoc := rec(TitlePage := rec(Abstract:= 
   "The autshift package is a GAP package containing methods for transducers that can be used\
     to represent isomorphisms between subshifts of finite type and related objects. It implements the\
    some of processes described in the papers https://arxiv.org/abs/2004.08478v4by and https://arxiv.org/abs/2112.13359\
    and builds on the existing package https://github.com/gap-packages/aaa.",
   Acknowledgements:= 
      "This package was created by Luke Elliott under the funding of ESPRC grant EP/R032866/1 and supervised by Collin Bleak. Special thanks to James Mitchell and Michael Torpey for their technical support.")),


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

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",
#AutoDoc
#    This is a record which can be used to control the scaffolding performed by
# AutoDoc, specifically to provide extra information for the title page. 
#For example, you can set AutoDoc.TitlePage.Copyright to a string which will then
#  be inserted on the generated title page. Using this method you can customize the following title page elements: TitleComment, Abstract, Copyright, Acknowledgements and Colophon.
#    Note that AutoDoc.TitlePage behaves exactly the same as the scaffold.TitlePage parameter of the AutoDoc (4.1-1) function.

AbstractHTML :=  
"The autshift package is a GAP package containing methods for transducers that can be used\
  to represent isomorphisms between subshifts of finite type and related objects. It implements the\
 some of processes described in the papers https://arxiv.org/abs/2004.08478v4by and https://arxiv.org/abs/2112.13359\
 and builds on the existing package https://github.com/gap-packages/aaa.",

PackageDoc := rec(
  BookName  := ~.PackageName,
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := ~.Subtitle,
),

Dependencies := rec(
  GAP := ">=4.8.0",
  NeededOtherPackages := [["automata", ">=0.0.0"],
                          ["digraphs", ">=0.15.0"],
			                    ["fr", ">=2.4.6"],
                          ["aaa", ">=0.0.0"],
                          ["datastructures", ">= 0.2.5"]],
  SuggestedOtherPackages := [],
  ExternalConditions := []),

##BannerString := Concatenation(
##  "----------------------------------------------------------------------",
##  "-------\n",
##  "Loading  autshift ", ~.Version, "\n",
##  "by ", ~.Persons[1].FirstNames, " ", ~.Persons[1].LastName,
##        " (", ~.Persons[1].WWWHome, ")\n",
##  "with contributions by:\n",
##  Concatenation(Concatenation(List(~.Persons{[2 .. Length(~.Persons) - 1]},
##       p -> ["     ", p.FirstNames, " ", p.LastName,
##       _RecogsFunnyNameFormatterFunction(
##         _RecogsFunnyWWWURLFunction(p)), ",\n"]))),
##  " and ", ~.Persons[Length(~.Persons)].FirstNames, " ",
##  ~.Persons[Length(~.Persons)].LastName,
##  _RecogsFunnyNameFormatterFunction(
##    _RecogsFunnyWWWURLFunction(~.Persons[Length(~.Persons)])), ".\n",
##  "-----------------------------------------------------------------------",
##  "------\n"),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));

