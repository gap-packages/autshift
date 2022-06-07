#
# autshift_new: implements algorithms for automorphisms of the shift and 
# related objects
#
# This file is a script which compiles the package manual.
# To complile the doc for aaa run the command AaaMakeDoc(); in gap.

LoadPackage( "AutoDoc" );
AutoDoc( rec( scaffold := true,
              autodoc := true ) );
QUIT;