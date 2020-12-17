IF OBJECT_ID('@resultsSchema.@prefiXgraph_events', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXgraph_events;

--CREATE TABLE @resultsSchema.@prefiXgraph_events as
    WITH ee AS (
    SELECT DISTINCT EVENTPERIOD_ID,CONCEPT_ID,date from @resultsSchema.@prefiXevents -- here is DISTINCT only to avoid duplicate events from two tables: drugs + drug_eras
    where CONCEPT_ID in (
        select e1_concept_id as e from @resultsSchema.@prefiXmylinks
        union
        select e2_concept_id as e from @resultsSchema.@prefiXmylinks)
    AND EVENTPERIOD_ID IN (SELECT EVENTPERIOD_ID FROM @resultsSchema.@prefiXevents WHERE CONCEPT_ID=@eventid {@limit == 0}?{}:{ LIMIT @limit } )
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


-- list E1->E2 eventpairs in the data. Takes only such eventpairs, where events occur right to next other (no intermediate significant events)
IF OBJECT_ID('@resultsSchema.@prefiXgraph_event_pairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXgraph_event_pairs;

--CREATE TABLE @resultsSchema.@prefiXgraph_event_pairs AS
    SELECT  a.EVENTPERIOD_ID,
            a.cohort_day as e1_cohort_day,
            b.cohort_day as e2_cohort_day,
            a.e as e1_concept_id,
            b.e as e2_concept_id,
            CASE WHEN a.e=@eventid THEN 1 ELSE 0 END AS E1_IS_INDEXEVENT,
            CASE WHEN b.e=@eventid THEN 1 ELSE 0 END AS E2_IS_INDEXEVENT
    INTO @resultsSchema.@prefiXgraph_event_pairs
    FROM
           @resultsSchema.@prefiXgraph_events a
           JOIN  @resultsSchema.@prefiXgraph_events b -- full join!
              ON  a.EVENTPERIOD_ID = b.EVENTPERIOD_ID
                  AND a.cohort_day + 1 = b.cohort_day -- only combine events that occur on next to each other
                  --AND CONCAT(a.event,'-',b.event) IN (SELECT CONCAT(e1_concept_id,'-',e2_concept_id) FROM @resultsSchema.@prefiXmylinks) --do not create event pairs that are not given in the original graph
    -- NB! No time limitations here between the events anymore...
    WHERE
      b.e IS NOT NULL
    ORDER BY a.EVENTPERIOD_ID,a.cohort_day,a.e,b.e
;

