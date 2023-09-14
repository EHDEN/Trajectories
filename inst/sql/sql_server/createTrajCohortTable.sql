-- Drop cohort table if it exists
IF OBJECT_ID('@resultsSchema.@prefiXtraj_cohort', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXtraj_cohort;

-- Create cohort table
CREATE TABLE @resultsSchema.@prefiXtraj_cohort (
  cohort_definition_id INT,
  cohort_start_date DATE,
  cohort_end_date DATE,
  subject_id BIGINT
);
