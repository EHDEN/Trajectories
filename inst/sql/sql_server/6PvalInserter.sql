UPDATE @resultsSchema.@prefixE1E2_model
SET
  event_pair_pvalue=@pval,
  EVENT_PAIR_RR=@rr,
  E2_COUNT_IN_CONTROL_GROUP=@E2_COUNT_IN_CONTROL_GROUP,
  POWER_ASSOCIATION=@power
WHERE E1_CONCEPT_ID = '@diag1' AND  E2_CONCEPT_ID = '@diag2';
