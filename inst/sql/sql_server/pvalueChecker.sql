SELECT event_pair_pvalue,directional_event_pair_pvalue FROM @resultsSchema.@prefixd1d2_model WHERE event1_concept_id = @diag1 AND  event2_concept_id = @diag2;
