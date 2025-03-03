/***
  DESCRIPTION:   IN-LINE macro logs path of macro to be compiled by AUTOCALL 
                 and return 0/1 IN-LINE to indicate whether macro exists     

  FILENAME:      assert_macro_exist.sas

  INPUT:         SYM  Name of the macro to look for in the SAS autocall paths.
                 Examples of use: 
                 Two options to log the location of an AUTOCALLED macro:
                 1. paste these lines into the definition of a macro:
                   %local thismac;
                   %let thismac = &sysmacroname;
                   %util_autocallpath(&thismac)
                 2. create log message only on 1st compilation by calling
                   %util_autocallpath(autocalled-macro-name)
                 in the autocalled file, but OUTSIDE the macro definition

  OUTPUT:       0 indicates failure to find macro in autocall paths
                1 indicates success, macro avail in autocall paths

  MACROS USED:   %util_resolve_sasautos

  ASSUMPTIONS/
  RESTRICTIONS:  + macro does not check that your SASAUTOS setting is right
                 NB: for windows, SASAUTOS literal pathnames must be in
                     DOUBLE-QUOTEs -- SAS cant handle single quotes

  AUTHOR:        D. Di Tommaso
  Acknowledgement: Inspired by FUTS system from Thotwave
                   http://thotwave.com/resources/futs-framework-unit-testing-sas/
***/

%macro assert_macro_exist(sym);
  %local OK paths sep idx current;

  %* WE havent found the source file yet *;
  %let OK = 0;

  %if %length(&sym) > 0 %then %do;
    %* GET current list of SASAUTOS filerefs (no quotes) & quoted pathnames *;
    %let paths = %util_resolve_sasautos;

    %* SET os-specific directory delimiter *;
    %if %index(&sysscp, WIN) %then %let sep = \;
    %else %let sep = /;

    %* FOR each path collected from sasautos, search for macro file *;
    %let idx = 1;
    %do %while (not &OK & %qscan(&paths,&idx,|) ne );
      %let current = %qscan(&paths,&idx,|);

      %if %sysfunc(fileexist( %quote(&current&sep&sym..sas) )) %then %do;
        %let OK = 1;
        %put NOTE: (ASSERT_MACRO_EXIST) PASS, found macro %upcase(&sym) in "&current&sep&sym..sas".;
      %end;

      %let idx = %eval(&idx+1);
    %end;

    %if not &OK %then %do;
      %put ERROR: (ASSERT_MACRO_EXIST) FAIL, unable to find macro %upcase(&sym).;
    %end;
  %end;
  %else %put ERROR: (ASSERT_MACRO_EXIST) FAIL, please provide a non-missing macro name.;

  &OK

%mend assert_macro_exist;
