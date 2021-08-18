UPDATE @resultsSchema.@prefixE1E2_model
SET
  RR=@rr,
  RR_CI_LOWER=@rr_ci_lower,
  RR_CI_UPPER=@rr_ci_upper,
  E2_PREVALENCE_IN_CONTROL_GROUP=@expected_prob,
  E2_PREVALENCE_IN_CASE_GROUP=@actual_prob,
  RR_POWER=@power,
  RR_PVALUE=@rr_pvalue,
  CONTROL_GROUP_SIZE=@control_group_size,
  CASE_GROUP_SIZE=@case_group_size,
  E2_COUNT_IN_CASE_GROUP=@num_observations_in_cases,
  E2_COUNT_IN_CONTROL_GROUP=@num_observations_in_controls
WHERE E1_CONCEPT_ID = '@diag1' AND  E2_CONCEPT_ID = '@diag2';
