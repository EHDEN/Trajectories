SELECT --a.event1_concept_id,
       --b.event2_concept_id,
       a.gender AS gender,
       a.age AS age,
       a.discharge_time AS discharge_time,
       c.cohort_count AS control_count, -- number of cohorts that have event pairs
       CASE WHEN d.cohort_count IS NULL THEN 0 ELSE d.cohort_count END AS control_d2, -- number of cohorts that have event2_concept_id as the second event of the event pair
       a.cohort_count AS case_count, -- number of cohorts that have event1_concept_id as the first event of the event pair
       CASE WHEN b.cohort_count IS NULL THEN 0 ELSE b.cohort_count END AS case_d2 -- number of cohorts that have exactly event1_concept_id->event2_concept_id event pair
  FROM
    @resultsSchema.@prefixd1_summary a
    LEFT JOIN (SELECT * FROM @resultsSchema.@prefixd1d2_summary WHERE event1_concept_id=@event1 and event2_concept_id=@event2) b
        ON a.gender=b.gender and a.age=b.age AND a.discharge_time=b.discharge_time
    LEFT JOIN @resultsSchema.@prefixsummary c
        ON a.gender=c.gender and a.age=c.age AND a.discharge_time=c.discharge_time
    LEFT JOIN (SELECT * FROM @resultsSchema.@prefixd2_summary WHERE event2_concept_id=@event2) d
        ON a.gender=d.gender and a.age=d.age AND a.discharge_time=d.discharge_time
  WHERE
        a.event1_concept_id=@event1
  ORDER BY age, gender, discharge_time;
