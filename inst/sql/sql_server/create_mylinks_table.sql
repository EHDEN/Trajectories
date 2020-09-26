IF OBJECT_ID('@resultsSchema.@prefiXmylinks', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXmylinks;

CREATE TABLE @resultsSchema.@prefiXmylinks (e1_concept_id INT, e2_concept_id INT);
