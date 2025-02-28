#!/usr/bin/env bash

WATCH_TABLE="$1"

module is-loaded postgresql || module load postgresql/13.0

watch "qstat | grep -Pi \"($1)|((?<=[\.]{3})[$1]+)\" | grep \"[R] batch\" | wc -l | xargs printf '%s jobs running\n';
qstat | grep -Pi \"($1)|((?<=[\.]{3})[$1]+)\" | grep \"[Q] batch\" | wc -l | xargs printf '%s jobs queued\n';
qstat | grep -Pi \"($1)|((?<=[\.]{3})[$1]+)\" | grep \"[H] batch\" | wc -l | xargs printf '%s jobs held\n\n';
ls /ngc/projects/ssi_mg/logs/*$WATCH_TABLE.log -1 2>/dev/null | tail; echo;
ls -1 /ngc/projects/ssi_mg/logs/*$WATCH_TABLE.log 2>/dev/null | tail -n 1 | xargs cat; echo;
psql -c \"SELECT date, start_time, duration, n_insertions, n_deactivations, success FROM prod.logs WHERE logs.table = ""'"$WATCH_TABLE"'"" ORDER BY start_time DESC LIMIT 25\""
