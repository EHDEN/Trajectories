-- If sqlRole is given then let's use it
{sqlRole != ''}?{SET ROLE @sqlRole;}

IF OBJECT_ID('@cohortTableSchema.@cohortTable', 'U') IS NOT NULL
  DROP TABLE @cohortTableSchema.@cohortTable;
