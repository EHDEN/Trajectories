SELECT * INTO
    @resultsSchema.@prefiXevents
FROM

    (
    -- cohorts
    SELECT
      e.subject_id                  AS PERSON_ID,
      c.eventperiod_id             AS EVENTPERIOD_ID,
      e.cohort_definition_id                  AS COHORT_ID,
      MIN(e.cohort_start_date)     AS date -- This is min date per one event-period (for patients with multiple event-periods, there are several min dates)
     -- for CDM 6 use condition_start_datetime?
    FROM @cohortDatabaseSchema.@cohortTableName e
    -- limit to cohorts defined for our trajectory analysis only
    INNER JOIN (SELECT * FROM @resultsSchema.@prefiXtraj_cohort_def WHERE COHORT_ID<>'@baseCohortId') d ON e.cohort_definition_id=d.COHORT_ID
    -- note that the same event may belong to several event periods. It gets multiplied here while doing this INNER JOIN
    INNER JOIN @resultsSchema.@prefiXtraj_base_cohort c ON e.subject_id=c.PERSON_ID {@daysBeforeIndexDate == Inf} ? {} : { AND DATEADD(day,@daysBeforeIndexDate,e.cohort_start_date)>=c.eventperiod_start_date } AND e.cohort_start_date<=c.eventperiod_end_date
    -- group by to get the minimum date per event type
    GROUP BY c.eventperiod_id,e.cohort_definition_id,e.subject_id

    ) SSS

;
