Using SAS autocall macro, buildfmt, to create formats

insired by
https://stackoverflow.com/questions/48472307/how-to-add-others-in-formats-in-sas

  All macros are on the end

      SAS provided macro buildFmt  (I fixed some errors in the macro)
      Can only do single value to single value no ranges)

      HOWEVER IS VERY EASY TO USE

         1. char to char
         2. char to num
         3. num to char
         4. Hardcoded  "Other= -1
         5. Places format in work.formats
         6. Does not support ranges
         7. Outputs to work.formats only

INPUT
=====

   sashelp.class       (char to char and char to num)
   sashelp.mileages    (num to char)


PROCESS and OUTPUT
==================

  CHAR TO CHAR Example  NAME TO SEX
  =========================

   %utl_buildFmt(
       formatName=gender
      ,lookupTable=sashelp.class
      ,startColumn=name
      ,labelColumn=sex
      );

   * CHECK;
   data want;
     set sashelp.class(keep=name obs=3) end=dne;
     sex=put(name,$gender.);
     if dne then do;
         name="Stormy";
         sex=put(name,$gender.);
     end;
   run;quit;

   WORK.WANT total obs=3

       NAME     SEX

      Alfred    M
      Alice     F
      STORMY    -1   * cannot change SAS sets to '-1'





  CHAR TO NUM Example  NAME TO AGE;
  =================================

   %utl_buildFmt(
       formatName=age
      ,lookupTable=sashelp.class
      ,startColumn=name
      ,labelColumn=age
   );

   * CHECK;
   data want;
     set sashelp.class(keep=name obs=3) end=dne;
     age=input(name,age.);
     if dne then do;
         name="Stormy";
         age=input(name,age.);
     end;
   run;quit;

   WORK.WANT total obs=19

      NAME       SEX

      Alfred     14
      Alice      13
      Stormy     -1    * cannot change SAS sets to -1





  NUM TO CHAR Example How far way are various cities from Atlanta;

   %utl_buildFmt(
        formatName=city
       ,lookupTable=sashelp.mileages
       ,startColumn=atlanta
       ,labelColumn=city
       );

   data want;
     set sashelp.mileages(keep=atlanta obs=3) end=dne;
     atlantac=put(atlanta,best5. -l);
     city=put(atlantac,city.);
     if dne then do;
         city=put('12345',city.);
     end;
   run;quit;

    Up to 40 obs WORK.WANT total obs=3

    Obs    ATLANTA    ATLANTAC    CITY

     1          0       0         Atlanta
     2        587       587       Chicago
     3       1212       1212      -1




*                _           _     _  __           _
 ___  __ _ ___  | |__  _   _(_) __| |/ _|_ __ ___ | |_
/ __|/ _` / __| | '_ \| | | | |/ _` | |_| '_ ` _ \| __|
\__ \ (_| \__ \ | |_) | |_| | | (_| |  _| | | | | | |_
|___/\__,_|___/ |_.__/ \__,_|_|\__,_|_| |_| |_| |_|\__|

;

/*****************************************************************/
 /* Copyright (c) 2003 by SAS Institute Inc., Cary, NC, USA. All  */
 /* Rights Reserved.                                              */
 /*****************************************************************/
 /* NAME:        buildfmt                                         */
 /*                                                               */
 /* DESCRIPTION: Used to create a format from a lookup table.     */
 /*              The format will either translate all the values  */
 /*              in the lookup table to "1"; or translate a value */
 /*              to the value of another column in the lookup     */
 /*              table.  All OTHER values will be set to "-1".    */
 /*                                                               */
 /* PRODUCT:     SES                                              */
 /*                                                               */
 /* USAGE:       buildFormat(formatName=,                         */
 /*                         lookupTable=,                         */
 /*                         startColumn=,                         */
 /*                         labelColumn=,                         */
 /*                         blanks=);                             */
 /*                                                               */
 /* formatName:   Name of the format that is created REQUIRED     */
 /*                                                               */
 /* lookupTable:  Name of the data table (libname.tablename) that */
 /*               will be used to create the format REQUIRED      */
 /*                                                               */
 /* startColumn:  Column in the lookup table to use as the start  */
 /*               value of the format - unformatted value         */
 /*               REQUIRED                                        */
 /*                                                               */
 /* labelColumn:  Column in the lookup table to use as the label  */
 /*               value of the format - formatted value           */
 /*               (required if this is a translation format)      */
 /*                                                               */
 /* blanks:       YES = blanks are valid values for this format   */
 /*               OPTIONAL - defaults to NO                       */
 /* ************************************************************* */

%macro utl_buildFmt (
      formatName=
     ,lookupTable=
     ,startColumn=
     ,labelColumn=
     ,blanks=NO
   );

  /* ******************************************************** */
  /* Validate that the parameters have valid values.          */
  /* ******************************************************** */

  /* formatName parm is required */
  %if (%str(&formatName) eq) %then
  %do;
    %put ERROR: formatName parameter is required.;
    %goto error;
  %end;

  /* lookupTable is required */
  %if (%str(&lookupTable) eq) %then
  %do;
    %put ERROR: lookupTable parameter is required.;
    %goto error;
  %end;

  %let blanks=%upcase(%str(&blanks));

  /* check for existence of lookup table */
  %if ((%str(&blanks) ne YES) and
       (not %sysfunc(exist(%str(&lookupTable))))) %then
  %do;
    %put ERROR: The lookupTable does not exist.;
    %goto error;
  %end;

  /* startColumn is required */
  %if (%str(&startColumn) eq) %then
  %do;
    %put ERROR: startColumn parameter is required.;
    %goto error;
  %end;

  /* check to see if start and label columns are in lookupTable, and */
  /* if there is data in the lookupTable.  Also determine the        */
  /* format type                                                     */
  %global formatType;
  %let formatType = N;
  %let startType = C;

  %let libref = %sysfunc(scan(&lookupTable, 1, %str(.)));
  %let table = %sysfunc(dequote(%sysfunc(scan(&lookupTable, 2, %str(.)))));
  %let lookupTable2 = &libref..&table;
  %let startColumn2 = %sysfunc(dequote(&startColumn));
  %let labelColumn2 = %sysfunc(dequote(&labelColumn));

  %let startType = N;
  %let labelType = N;
  %let anyRecords = 0;

  /* open the lookup table and see if it has the start and label columns, */
  /* and if it has any records                                            */
  data _null_;
    dsid = open("&lookupTable2");
    call symput("dsid", compress(put(dsid, 10.)));

    if (dsid gt 0) then
    do;  /* data set was opened successfully */

      varnum = varnum(dsid, "&startColumn2");
      call symput("startVar", compress(put(varnum, 10.)));
      if (varnum gt 0) then
        call symput("startType", vartype(dsid, varnum));

      varnum = varnum(dsid, "&labelColumn2");
      call symput("labelVar", compress(put(varnum, 10.)));
      if (varnum gt 0) then
        call symput("labelType", vartype(dsid, varnum));

      call symput("anyRecords", put(attrn(dsid, "ANY"), 1.));

      dsid = close(dsid);

    end; /* data set was opened successfully */

  run;

  %if (&dsid gt 0) %then
  %do;  /* lookup table was successfully opened */

    %if &startVar eq 0 %then
    %do;  /* startColumn does not exist */
      %put ERROR: %str(&startColumn) is not found in table %str(&lookupTable).;
      %goto error;
    %end; /* startColumn does not exist */

    %let formatType = C;
    %let catType = FORMATC;

    /* check for labelColumn - if not blank then this is a translation format */
    %if (%str(&labelColumn) ne) %then
    %do;  /* label column is not blank */

      %if &labelVar eq 0 %then
      %do;  /* labelColumn does not exist */
        %put ERROR: %str(&labelColumn) is not found in table %str(&lookupTable).;
        %goto error;
      %end; /* labelColumn does not exist */

      /* if the label is numeric, then this should be an informat, not a format */
      %if (&labelType eq N) %then
      %do;
        %let formatType = I;
        %let catType = INFMT;
      %end;

    %end; /* labelColumn is not blank */

    %else
    %do;  /* labelColumn is blank - this is a lookup */
      %let formatType = I;
      %let catType = INFMT;
    %end;

    /* check for observations in the lookup table */
    %if ((&anyRecords ne 1) and
         (%str(&blanks) ne YES)) %then
    %do;  /* there is no data in the lookup table */
      %put ERROR: Table, %str(&lookupTable), has no data.;
      %goto error;
    %end; /* there is no data in the lookup table */

  %end;  /* lookup table was successfully opened */

  %else
  %do;  /* lookupTable did not open successfully */
    %put ERROR: The lookupTable could not be opened.;
    %goto error;
  %end; /* lookupTable did not open successfully */

  /* character format */
  %if (&formatType eq C) %then
  %do;  /* character format */

    %if (%length(%str(&formatName)) gt 31) %then
    %do;  /* name is too long */
      %put ERROR: The formatName cannot be longer than 31 characters;
      %goto error;
    %end; /* name is too long */

    /* character format names must begin with a $ */
    %if (%substr(%str(&formatName), 1, 1) ne %str($)) %then
      %let formatName = %str($)%str(&formatName);

  %end; /* character format */

  /* numeric format */
  %else
  %do;  /* numeric format */

    %if (%length(%str(&formatName)) gt 32) %then
    %do;  /* name is too long */
      %put ERROR: The formatName cannot be longer than 32 characters;
      %goto error;
    %end; /* name is too long */

    /* numeric formats cannot begin with a $ */
    %if (%substr(%str(&formatName), 1, 1) eq %str($)) %then
      %let formatName = %substr(%str(&formatName), 2);

  %end; /* numeric format */

  %if (&catType eq FORMATC) %then
    %let catFormatName = %substr(%str(&formatName), 2);
  %else
    %let catFormatName = %str(&formatName);

  /* ******************************************************** */
  /* Create the format.                                       */
  /* ******************************************************** */

  /* Create cntlin data table for input to the proc format.   */
  data fmtTable (keep=fmtName start label type hlo);

    length start $32
    %if (&formatType eq I) %then
    %do;
       label 8;
    %end;
    %else
    %do;
       label $32;
    %end;

    set &lookupTable end=eof;

    /* set the start variable */
    %if (&startType eq C) %then
    %do;
      start = &startColumn;
    %end;
    %else
    %do; /* else - numeric start */
      start = left(put(&startColumn, 32.));
      if start eq "." then
        start = "";
    %end;  /* else - numeric start */

    /* set the label variable */
    %if (%str(&labelColumn) ne) %then
    %do;  /* using a label column */
      label = &labelColumn;
    %end;  /* using a label column */
    %else
    %do;   /* no label column */
      %if (&formatType eq I) %then
      %do;
        label = 1;
      %end;
      %else
      %do;
        label = "1";
      %end;
    %end; /* no label column */

    fmtName = "&formatName";
    type = "&formatType";

    output;

    if eof then
    do;  /* end of file */

      /* if blanks are a valid value, then add to the format */
      %if (%str(&blanks) eq YES) %then
      %do;
        start = "";
        %if (&formatType eq I) %then
        %do;
          label = 1;
        %end;
        %else
        %do;
          label = "1";
        %end;
        output;
      %end;

      /* "OTHER" values */
      start = "-9999999";
      hlo = "O";
      %if (&formatType eq I) %then
      %do;
        label = -1;
      %end;
      %else
      %do;
        label = "-1";
      %end;

      output;

    end; /* end of file */

  run;

  %if (&syserr gt 4) %then
    %goto error;

  /* delete any duplicates (proc format will fail otherwise) */
  proc sort data=fmtTable nodupkey;
    by start;
  run;

  %if (&syserr gt 4) %then
    %goto error;

  /* delete format if it already exists */
  %if %sysfunc(cexist(WORK.FORMATS.&catFormatName..&catType)) %then
  %do;
    proc catalog cat=WORK.FORMATS;
      delete &catFormatName (et=&catType);
    run;
  %end;

  /* create format */
  proc format cntlin=fmtTable library=work;
  run;

  %if (&syserr gt 4) %then
    %goto error;

  /* delete fmtTable data table */
  proc datasets lib=WORK nolist;
    delete fmtTable;
  quit;

  %goto macroEnd;

%error:
  %let errorflag = 1;
  %let sysrc = 9999;

%macroEnd:
%mend utl_buildfmt;
