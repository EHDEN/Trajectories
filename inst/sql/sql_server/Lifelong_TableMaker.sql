------------------------------------------------------------

set role @role_for_writing;

-------------------------------------------------------------------
--  Seleting patients with correct patient_id and gender
-- (Age needs to be rounded down before aggregation query)
-------------------------------------------------------------------
drop table if exists @schema_for_writing.@prefix_patient_info;

CREATE TABLE @schema_for_writing.@prefix_patient_info AS

    SELECT p.person_id
           ,p.year_of_birth
           ,case when p.gender_concept_id = 8532
               then 'F'
               when p.gender_concept_id = 8507
                then 'M'
                end as gender
          ,c.condition_concept_id as diagnosis
          ,c.condition_start_date as date
    FROM @exposure_database_schema.person as p
      JOIN @exposure_database_schema.condition_occurrence as c
          ON p.person_id=c.person_id
    WHERE (p.gender_concept_id = 8532 or p.gender_concept_id = 8507)
      AND p.person_id IS NOT NULL
      AND p.person_id > 0
      AND c.condition_concept_id > 0;

--adding age during diagnosis
ALTER TABLE  @schema_for_writing.@prefix_patient_info ADD COLUMN age INTEGER;
UPDATE  @schema_for_writing.@prefix_patient_info SET
    age = cast(substring(cast(date as text),1,4) as numeric) - year_of_birth;

-------------------------------------------------------------------
-- Adding diagnosis info
-- array_to_string added to eliminate null values from the array
-------------------------------------------------------------------
drop table if exists @schema_for_writing.@prefix_all_diagnoses_grouped;

CREATE TABLE @schema_for_writing.@prefix_all_diagnoses_grouped AS
    SELECT
           person_id
           ,gender
           ,age
           ,array_agg(DISTINCT diagnosis) as diagnoses

    FROM @schema_for_writing.@prefix_patient_info
    GROUP BY
             person_id
            ,gender
            ,age
    ORDER BY person_id;


------------------------------------------------------------
--   Psedonymise patient IDs -SKIPPING
------------------------------------------------------------
/*
drop table if exists @schema_for_writing.@prefix_distinct_patients;

CREATE TABLE @schema_for_writing.@prefix_distinct_patients as
    SELECT DISTINCT person_id as person_id
                    --,random() as random
    FROM @schema_for_writing.@prefix_all_diagnoses_by_year;

drop table if exists @schema_for_writing.tmp_rand_numbers;
CREATE TABLE @schema_for_writing.tmp_rand_numbers as
	SELECT
		row_number() over (order by random()) as person_id,
		person_id as true_person_id
    FROM @schema_for_writing.@prefix_distinct_patients
    order by random();

------------------------------------------------------------
--             Psedonymise data
------------------------------------------------------------

drop table if exists trajectories.hmm_arrayed_diagnoses_by_year;

CREATE TABLE trajectories.hmm_arrayed_diagnoses_by_year as
SELECT
	tbl2.person_id,
	tbl1.primary_diagnoses,
	tbl1.secondary_diagnoses,
    tbl1.gender,
    tbl1.age

FROM @schema_for_writing.@prefix_all_diagnoses_by_year as tbl1
LEFT JOIN @schema_for_writing.tmp_rand_numbers as tbl2
    ON tbl1.person_id = tbl2.true_person_id
ORDER BY tbl2.person_id, tbl1.age;
*/
------------------------------------------------------------
--   dropping unnecessary tables
------------------------------------------------------------


drop table if exists @schema_for_writing.@prefix_patient_info;


