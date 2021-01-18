SELECT
  E1_CONCEPT_ID,
  E2_CONCEPT_ID,
  E1_E2_EVENTPERIOD_COUNT,
  RR_IN_PREVIOUS_STUDY
FROM @resultsSchema.@prefixE1E2_model
  WHERE
  ( 1=@forceRecalculation -- if forceRecalculation is TRUE, then this condition is added
    AND
    E1_E2_EVENTPERIOD_COUNT>0
  ) OR ( 0=@forceRecalculation -- if forceRecalculation is FALSE, then this additional limiting condition is added to skip event pairs for whose the pvalue is already calculated
    AND
    E1_E2_EVENTPERIOD_COUNT>0
    AND
    EVENT_PAIR_PVALUE IS NULL OR (EVENT_PAIR_PVALUE<=@cutoffPval AND DIRECTIONAL_EVENT_PAIR_PVALUE IS NULL)
  )
ORDER BY
  E1_E2_EVENTPERIOD_COUNT
DESC;
