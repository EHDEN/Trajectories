SELECT
  *
  FROM @resultsSchema.@prefixE1E2_model
WHERE
  E1_BEFORE_E2_COUNT_IN_EVENTS>0 AND
  CASE
    WHEN (RR_IN_PREVIOUS_STUDY IS NULL OR RR_IN_PREVIOUS_STUDY=0) -- Power is always sufficient if RR in previous study is not given
      THEN TRUE
    WHEN RR_POWER>=0.8 -- Power is sufficient if RR in previous study is given and it is >=80%
      THEN TRUE
    ELSE FALSE -- Otherwise the power is not sufficient
  END
  AND
   (
    ( 1=@forceRecalculation ) -- if forceRecalculation is TRUE, then this condition is added
    OR
    ( 0=@forceRecalculation AND RR_PVALUE IS NULL) -- if forceRecalculation is FALSE, then this additional limiting condition is added to skip event pairs for whose the pvalue is already calculated
  )
  ;
