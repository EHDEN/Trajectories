IF OBJECT_ID('@resultsSchema.@prefiXeventperiods_with_E1', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXeventperiods_with_E1;
IF OBJECT_ID('@resultsSchema.@prefiXeventperiods_with_E1E2', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXeventperiods_with_E1E2;
IF OBJECT_ID('@resultsSchema.@prefiXE1_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1_summary;
IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model;
IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model_input', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model_input;
IF OBJECT_ID('@resultsSchema.@prefiXE1E2_model_temp', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_model_temp;
IF OBJECT_ID('@resultsSchema.@prefiXE1E2_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE1E2_summary;
IF OBJECT_ID('@resultsSchema.@prefiXE2_summary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXE2_summary;
IF OBJECT_ID('@resultsSchema.@prefiXdebug', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXdebug;
IF OBJECT_ID('@resultsSchema.@prefiXcohort', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXcohort;
IF OBJECT_ID('@resultsSchema.@prefiXmycohort', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXmycohort;
IF OBJECT_ID('@resultsSchema.@prefiXevent_counts_in_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevent_counts_in_eventperiods;
IF OBJECT_ID('@resultsSchema.@prefiXevents', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents;
IF OBJECT_ID('@resultsSchema.@prefiXevents_in_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXevents_in_eventperiods;
IF OBJECT_ID('@resultsSchema.@prefiXpairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs;
IF OBJECT_ID('@resultsSchema.@prefiXsingle_event_eventperiods', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsingle_event_eventperiods;
IF OBJECT_ID('@resultsSchema.@prefiXsummary', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXsummary;
IF OBJECT_ID('@resultsSchema.@prefiXgraph_events', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXgraph_events;
IF OBJECT_ID('@resultsSchema.@prefiXgraph_event_pairs', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXgraph_event_pairs;
IF OBJECT_ID('@resultsSchema.@prefiXmylinks', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXmylinks;
IF OBJECT_ID('@resultsSchema.@prefiXpairs_of_model', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_of_model;
IF OBJECT_ID('@resultsSchema.@prefiXpairs_stat', 'U') IS NOT NULL
  DROP TABLE @resultsSchema.@prefiXpairs_stat;
