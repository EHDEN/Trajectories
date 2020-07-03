UPDATE @resultsSchema.@prefixd1d2_model SET event_pair_pvalue=@pval, event_pair_effect=@effect WHERE event1_concept_id = @diag1 AND  event2_concept_id = @diag2;
