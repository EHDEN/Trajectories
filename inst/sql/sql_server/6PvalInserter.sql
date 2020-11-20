UPDATE @resultsSchema.@prefixE1E2_model SET event_pair_pvalue=@pval, event_pair_effect=@effect WHERE E1_CONCEPT_ID = @diag1 AND  E2_CONCEPT_ID = @diag2;
