IF OBJECT_ID('@resultsSchema.@prefiXgraph_events', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXgraph_events;

--CREATE TABLE @resultsSchema.@prefiXgraph_events as
    WITH ee AS (
    SELECT DISTINCT EVENTPERIOD_ID,CONCEPT_ID,date from @resultsSchema.@prefiXevents -- here is DISTINCT only to avoid duplicate events from two tables: drugs + drug_eras
    where CONCEPT_ID in (
        select e1_concept_id as e from @resultsSchema.@prefiXmylinks
        union
        select e2_concept_id as e from @resultsSchema.@prefiXmylinks)
)
SELECT ee.EVENTPERIOD_ID,
       ee.CONCEPT_ID as e,
       ee2.cohort_day
INTO @resultsSchema.@prefiXgraph_events
FROM ee
    LEFT JOIN (
    SELECT ee.EVENTPERIOD_ID,
           ee.date,
           RANK() OVER (
               PARTITION BY EVENTPERIOD_ID
               ORDER BY date
               ) AS cohort_day -- gives the order number for each day of the patient records. The problem is that if several events occur on a same day, the ranks are given like this 1, 1, 3, 4, 5
    from ee
    GROUP BY ee.EVENTPERIOD_ID, ee.date
) ee2 ON ee.EVENTPERIOD_ID=ee2.EVENTPERIOD_ID and ee.date=ee2.date
ORDER BY ee.EVENTPERIOD_ID,cohort_day;


-- list E1->E2 eventpairs in the data (satisfying all study parameters). Takes only such eventpairs, where events occur right to next other (no intermediate significant events)
IF OBJECT_ID('@resultsSchema.@prefiXgraph_event_pairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXgraph_event_pairs;

SELECT  a.EVENTPERIOD_ID,
        --a.e1_date as e1_cohort_day,
        --a.e2_date as e2_cohort_day,
        a.e1_concept_id as e1_concept_id,
        a.e2_concept_id as e2_concept_id
INTO @resultsSchema.@prefiXgraph_event_pairs
FROM
       @resultsSchema.@prefiXpairs a
       INNER JOIN @resultsSchema.@prefiXmylinks l
        ON a.e1_concept_id=l.e1_concept_id AND a.e2_concept_id=l.e2_concept_id
ORDER BY a.EVENTPERIOD_ID,
  --a.e1_date,a.e2_date,
  a.e1_concept_id,a.e2_concept_id
;

CREATE INDEX @prefiXgraph_event_pairs_idx ON @resultsSchema.@prefiXgraph_event_pairs (E1_CONCEPT_ID,E2_CONCEPT_ID);

