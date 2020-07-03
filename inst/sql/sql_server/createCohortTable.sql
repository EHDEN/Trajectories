-- Drop cohort table if it exists
IF OBJECT_ID('@cohortTableSchema.@cohortTable', 'U') IS NOT NULL
  DROP TABLE @cohortTableSchema.@cohortTable;

-- Create cohort table
CREATE TABLE @cohortTableSchema.@cohortTable (
  cohort_definition_id INT,
  cohort_start_date DATE,
  cohort_end_date DATE,
  subject_id BIGINT
);
