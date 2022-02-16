#
# autshift_new: implements algorithms for automorphisms of the shift and 
# related objects
#
# This file is a script which compiles the package manual.
# To complile the doc for aaa run the command AaaMakeDoc(); in gap.

if not IsDirectoryPath("gap")
    or not "transducer.gd" in DirectoryContents("gap") then
  Print("Error: GAP must be run from the autshift package directory ",
        "when reading makedoc.g\n");
  FORCE_QUIT_GAP(1);
fi;
pkgdir := DirectoryCurrent();

PACKAGE := "autshift";
LoadPackage("GAPDoc");

_DocXMLFiles := ["main.xml",
                 "z-chap0.xml",
                 "z-chap1.xml",
                 "z-chap2.xml",
                 "z-chap3.xml",
                 "z-chap4.xml",
                 "z-chapint.xml",
                 "title.xml",
                 "shiftoperations.xml",
                 "transducer.xml",
                 "utils.xml",
                 "z-autshiftbib.xml",
                 "../PackageInfo.g"];


MakeGAPDocDoc(Filename(pkgdir, "doc"),
              "main.xml", _DocXMLFiles, PACKAGE, "MathJax", "../../..");
CopyHTMLStyleFiles(Filename(pkgdir, "doc"));
GAPDocManualLabFromSixFile(PACKAGE, Filename(pkgdir, "doc/manual.six"));

QUIT;
