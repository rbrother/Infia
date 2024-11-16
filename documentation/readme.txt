Infia version 980911
====================

When unzipping this distrubution, use the option to re-create the
original directory structure.

This version of Infia has gone through major internal changes and is still
under construction. The major modification is that the Borland databases 
that the program has used to store analysis data have been replaced by the
combination of ASCII comma separated values files and internal object 
databases. These modifiactions have lead to increased efficiency and robustness.

However, the modifications are still under way and this version can be
considered only an early alpha version. Features that have not yet been
re-implemented in this version include the non-linear least squares
calculation for optimisation of resonance systems. Updated in near future
should bring these features back to the program.

Contents of the distribution
============================

INFIA.EXE     The main executable, 32-bit Windows95/NT4 program
MATHDLL.DLL   Dynamic link library with some mathematical routines. Must be
              included in the program directory.
ARTICLE.PS    Post Script version of the article that describes the program.
              This is not a user guide but rather a description of the
              programs capabilities from scientific point of view. 
              Highly recommended reading!
MANUAL.DOC    A highly preliminary users manual. Important to to read!
CO_Demo/      
DCCBr_Demo/   Directory containing project files of a demonstration project
              involving spectrum of the nu4 band system of DCCBr. The main
              project file to be loaded with the program is nu4.prj. It is
              recommended that one plays around with the demonstration
              project and studies its files.