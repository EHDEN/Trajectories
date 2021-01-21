UPDATE @resultsSchema.@prefixE1E2_model
SET
  EVENT_PAIR_PVALUE=@pval,
  EVENT_PAIR_PVALUE_SIGNIFICANT=@pvalSignificant,
  DIRECTIONAL_EVENT_PAIR_PVALUE=NULL, --reset this to prevent having some old value given here even if assocication test is not significant
  DIRECTIONAL_EVENT_PAIR_PVALUE_SIGNIFICANT=NULL --reset this to prevent having some old value given here even if assocication test is not significant
WHERE E1_CONCEPT_ID = '@diag1' AND  E2_CONCEPT_ID = '@diag2';
