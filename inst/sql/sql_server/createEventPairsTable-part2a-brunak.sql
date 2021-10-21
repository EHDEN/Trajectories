---------------------------------------------------------------------
-- Create separate table for event pairs that are going to be analysed
-- This is just a temporary table to serve as an entry point when the package is run in validation mode (then the pairs will be loaded to this table)
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXE1E2_model_input...');

IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model_input', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model_input;
SELECT
        E1_CONCEPT_ID,
        E2_CONCEPT_ID
INTO @resultsSchema.@prefiXE1E2_model_input
FROM
 @resultsSchema.@prefiXpairs
GROUP BY
  E1_CONCEPT_ID,
  E2_CONCEPT_ID
HAVING
  count(*) >= @minPatientsPerEventPair -- Minimum limit
ORDER BY
  E1_CONCEPT_ID,
  E2_CONCEPT_ID;


ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E1_NAME VARCHAR(255) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E1_DOMAIN VARCHAR(20) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E2_NAME VARCHAR(255) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E2_DOMAIN VARCHAR(20) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD RR_IN_PREVIOUS_STUDY DECIMAL NULL;

--- Instead of using concept id-s, add concept names + concept domains
UPDATE @resultsSchema.@prefiXE1E2_model_input
SET E1_NAME = SUBSTRING(name,1,220),
E1_DOMAIN = 'Condition'
FROM classifications.icd10_parent_chain i WHERE i.code=E1_CONCEPT_ID;


UPDATE @resultsSchema.@prefiXE1E2_model_input
SET E2_NAME = SUBSTRING(name,1,220),
E2_DOMAIN = 'Condition'
FROM classifications.icd10_parent_chain i WHERE i.code=E2_CONCEPT_ID;
