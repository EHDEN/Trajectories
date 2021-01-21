SELECT
  *
  FROM @resultsSchema.@prefixE1E2_model
WHERE
  EVENT_PAIR_PVALUE_SIGNIFICANT='*'
  AND
  (
    ( 1=@forceRecalculation ) -- if forceRecalculation is TRUE, then this condition is added
    OR
    ( 0=@forceRecalculation AND DIRECTIONAL_EVENT_PAIR_PVALUE IS NULL) -- if forceRecalculation is FALSE, then this additional limiting condition is added to skip event pairs for whose the pvalue is already calculated
  )
;
