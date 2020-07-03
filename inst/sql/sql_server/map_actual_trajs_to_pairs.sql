-- If sqlRole is given then let's use it
{sqlRole != ''}?{SET ROLE @sqlRole;}


---------------------------------------------------------------------------------------
-- Create a temporary table for debugging
---------------------------------------------------------------------------------------
IF OBJECT_ID('@resultsSchema.@prefiXevents_significant', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_significant;

CREATE TABLE @resultsSchema.@prefiXevents_significant as
    WITH ee AS (
    SELECT * from @resultsSchema.@prefiXevents
    where dgn in (
        select event1_concept_id as event from @resultsSchema.@prefiXd1d2_model where directional_event_pair_pvalue <= @cutoff_val and event_pair_effect >= @effectSize
        union
        select event2_concept_id as event from @resultsSchema.@prefiXd1d2_model where directional_event_pair_pvalue <= @cutoff_val and event_pair_effect >= @effectSize)
)
SELECT ee.cohort_id,
       ee.dgn as event,
       ee2.cohort_day
FROM ee
    LEFT JOIN (
    SELECT ee.cohort_id,
           ee.date,
           RANK() OVER (
               PARTITION BY cohort_id
               ORDER BY date
               ) AS cohort_day -- gives the order number for each day of the patient records. The problem is that if several events occur on a same day, the ranks are given like this 1, 1, 3, 4, 5
    from ee
    GROUP BY ee.cohort_id, ee.date
) ee2 ON ee.cohort_id=ee2.cohort_id and ee.date=ee2.date
ORDER BY cohort_id,cohort_day;


-- list E1->E2 eventpairs in the data. Takes only such eventpairs, where events occur right to next other (no intermediate significant events)
IF OBJECT_ID('@resultsSchema.@prefiXexact_sign_pairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXexact_sign_pairs;

CREATE TABLE @resultsSchema.@prefiXexact_sign_pairs AS
    SELECT  a.cohort_id,
            a.event as event1_concept_id,
            b.event as event2_concept_id
    FROM
           @resultsSchema.@prefiXevents_significant a
           JOIN  @resultsSchema.@prefiXevents_significant b -- full join!
              ON  a.cohort_id = b.cohort_id
                  AND a.cohort_day + 1 = b.cohort_day -- only combine events that occur on next to each other
    -- NB! No time limitations here between the events anymore...
    WHERE
      b.event IS NOT NULL
;


-- count E1->E2 eventpairs in the data. Counts only such eventpairs, where events occur right to next other (no intermediate significant events)
IF OBJECT_ID('@resultsSchema.@prefiXexact_sign_pair_counts', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXexact_sign_pair_counts;

CREATE TABLE @resultsSchema.@prefiXexact_sign_pair_counts AS
    SELECT  event1_concept_id,
            event2_concept_id,
            COUNT(*) as count
    FROM
           @resultsSchema.@prefiXexact_sign_pairs
    GROUP BY event1_concept_id,event2_concept_id
;

-- If sqlRole is given then let's use it
{sqlRole != ''}?{RESET ROLE;}

-- put the results also to D1D2_model table
UPDATE @resultsSchema.@prefiXd1d2_model
  SET COHORT_COUNT_HAVING_E2_RIGHT_AFTER_E1=@prefiXexact_sign_pair_counts.count
  FROM @resultsSchema.@prefiXexact_sign_pair_counts
  WHERE @prefiXd1d2_model.event1_concept_id=@prefiXexact_sign_pair_counts.event1_concept_id
    AND @prefiXd1d2_model.event2_concept_id=@prefiXexact_sign_pair_counts.event2_concept_id

