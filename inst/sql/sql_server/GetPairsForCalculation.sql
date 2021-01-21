SELECT
  E1_CONCEPT_ID,
  E2_CONCEPT_ID,
  E1_COUNT,
  E1_COUNT_AS_FIRST_EVENT,
  E2_COUNT,
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
    E2_PREVALENCE_IN_CONTROL_GROUP IS NULL
  )
ORDER BY
  E1_E2_EVENTPERIOD_COUNT
DESC;
