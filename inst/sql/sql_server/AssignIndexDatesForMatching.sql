IF OBJECT_ID('@resultsSchema.@prefiXmatching', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXmatching;



SELECT * INTO
@resultsSchema.@prefiXmatching
FROM

(

SELECT
  index_dates.eventperiod_id AS EVENTPERIOD_ID,
  index_dates.is_case AS IS_CASE,
  e.gender AS GENDER,
  index_dates.index_date AS INDEX_DATE,
  e.age AS AGE,
  DATEDIFF(DAY, mc.eventperiod_start_date, index_dates.index_date) AS LEN_HISTORY_DAYS,
  DATEDIFF(DAY, index_dates.index_date, mc.eventperiod_end_date) AS LEN_FOLLOWUP_DAYS
FROM
(

SELECT c.eventperiod_id,
      --c.e1_date,
      --random_dates.random_date,
      CASE WHEN c.e1_date IS NOT NULL THEN 1 ELSE 0 END AS is_case,
      CASE WHEN c.e1_date IS NOT NULL THEN c.e1_date ELSE random_dates.random_date END AS index_date
      FROM
          (
                                SELECT eventperiod_id,
                                    MAX(case when concept_id='@diagnosis1' then date else NULL end) AS e1_date
                                 FROM
                                  @resultsSchema.@prefiXevents_in_eventperiods e
                                 GROUP BY
                                  eventperiod_id
          ) c

                                  LEFT JOIN
          (
                                  SELECT eventperiod_id,date as random_date FROM
                                    (SELECT eventperiod_id,date, ROW_NUMBER() OVER (PARTITION BY eventperiod_id ORDER BY RAND()) AS rn FROM @resultsSchema.@prefiXevents_in_eventperiods) tmp
                                    WHERE rn<=1
           ) random_dates
          ON c.eventperiod_id=random_dates.eventperiod_id

) index_dates

LEFT JOIN

(

        SELECT eventperiod_id,date,MAX(gender) AS gender,MAX(age) AS age FROM @resultsSchema.@prefiXevents_in_eventperiods GROUP BY eventperiod_id,date

) e
ON index_dates.eventperiod_id=e.eventperiod_id AND index_dates.index_date=e.date

LEFT JOIN

@resultsSchema.@prefiXmycohort mc ON index_dates.eventperiod_id=mc.eventperiod_id

) alldata;

