UPDATE @resultsSchema.@prefixE1E2_model
SET
  RR=@rr,
  RR_CI_LOWER=@rr_ci_lower,
  RR_CI_UPPER=@rr_ci_upper,
  E2_PREVALENCE_IN_CONTROL_GROUP=@expected_prob,
  E2_PREVALENCE_IN_CASE_GROUP=@actual_prob,
  RR_POWER=@power
WHERE E1_CONCEPT_ID = '@diag1' AND  E2_CONCEPT_ID = '@diag2';
