SELECT --a.event1_concept_id,
       --b.event2_concept_id,
       a.gender AS gender,
       a.age AS age,
       a.discharge_time AS discharge_time,
       c.eventperiod_count AS control_count, -- number of eventperiods that have event pairs
       CASE WHEN d.eventperiod_count IS NULL THEN 0 ELSE d.eventperiod_count END AS control_d2, -- number of eventperiods that have event2_concept_id as the second event of the event pair
       a.eventperiod_count AS case_count, -- number of eventpairs that have event1_concept_id as the first event of the event pair
       CASE WHEN b.eventperiod_count IS NULL THEN 0 ELSE b.eventperiod_count END AS case_d2 -- number of eventperiods that have exactly event1_concept_id->event2_concept_id event pair
  FROM
    @resultsSchema.@prefixE1_summary a
    LEFT JOIN (SELECT * FROM @resultsSchema.@prefixE1E2_summary WHERE E1_CONCEPT_ID=@event1 and E2_CONCEPT_ID=@event2) b
        ON a.gender=b.gender and a.age=b.age AND a.discharge_time=b.discharge_time
    LEFT JOIN @resultsSchema.@prefixsummary c
        ON a.gender=c.gender and a.age=c.age AND a.discharge_time=c.discharge_time
    LEFT JOIN (SELECT * FROM @resultsSchema.@prefixE2_summary WHERE E2_CONCEPT_ID=@event2) d
        ON a.gender=d.gender and a.age=d.age AND a.discharge_time=d.discharge_time
  WHERE
        a.E1_CONCEPT_ID=@event1
  ORDER BY age, gender, discharge_time;
