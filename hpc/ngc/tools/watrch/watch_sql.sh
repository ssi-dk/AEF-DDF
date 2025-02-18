#!/usr/bin/env bash

WATCH_TABLE="$1"

module is-loaded postgresql || module load postgresql/13.0
[ ! -n "$DB_HOST" ] && DB_HOST="$PGHOST"

watch "psql -c \"SELECT
    query_start::TIME(0),
    state_change::TIME(0),
    (CASE
      WHEN state = 'active' THEN NOW()
      ELSE state_change
    END - query_start)::TIME(0) runtime,
    LEFT(query, 100) query
FROM pg_stat_activity WHERE usename = '$USER' AND state = 'active'\""
