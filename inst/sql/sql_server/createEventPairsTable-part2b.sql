---------------------------------------------------------------------
-- Create separate table for event pairs that are going to be analysed
-- This is just a temporary table to serve as an entry point when the package is run in validation mode (then the pairs will be loaded to this table)
----------------------------------------------------------------------

INSERT INTO @resultsSchema.@prefiXdebug (entry) VALUES ('Creating @resultsSchema.@prefiXE1E2_model_input...');

IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model_input', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model_input;

CREATE TABLE @resultsSchema.@prefiXE1E2_model_input (
e1_concept_id INT,
e2_concept_id INT
);

ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E1_NAME VARCHAR(255) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E1_DOMAIN VARCHAR(20) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E2_NAME VARCHAR(255) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD E2_DOMAIN VARCHAR(20) NULL;
ALTER TABLE @resultsSchema.@prefiXE1E2_model_input ADD RR_IN_PREVIOUS_STUDY DECIMAL NULL;
