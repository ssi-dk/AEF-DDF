#!/bin/bash

{ module reload; module load postgresql/15.1; } >/dev/null

while getopts "s:t:d:" opt; do
	case "$opt" in
		s) SCHEMA="${OPTARG}" ;;
		t) TABLE="${OPTARG}" ;;
		d) DATETIME=${OPTARG} ;;
	esac
done

for var in "SCHEMA" "TABLE" "DATETIME"; do
	if [ ! -n "${!var}" ]; then
		echo 'Variable $'$var' not set!' >&2
		exit 1 
	fi
done

# Test and expand datetime input
export DATETIME="$(date +"%Y-%m-%d %H:%M:%S" -d "$DATETIME")" || exit 1
export SCHEMA="$SCHEMA"
export TABLE="$TABLE"

echo DATETIME=$DATETIME
echo SCHEMA=$SCHEMA
echo TABLE=$TABLE

# Retrieve log files to delete
LOG_FILES="$(psql -tc "$(envsubst <<< "
SELECT
  log_file
FROM prod.logs

WHERE COALESCE(\"schema\", 'prod') = '$SCHEMA'
  AND \"table\" = '$TABLE'
  AND \"date\" >= '$DATETIME'
")")"

echo -e LOG_FILES:"\n$LOG_FILES\n"

# Get user confirmation in interactive session
if [ -t 0 ]; then
	read -n 1 -p "Do you wish to proceed? (y/N) " yn
	echo
	if [[ "$yn" != [Yy] ]]; then exit 1; fi
else
	echo
fi

# Backup crontab and restore if anything goes wrong
restore_crontab() {
	echo 'Restoring crontab!'
	crontab ~/.crontab.bak
	
	exit 1
}

crontab -l > ~/.crontab.bak
trap 'restore_crontab' ERR
crontab -r

LOG_FOLDER="/ngc/projects/ssi_mg/logs"
for logfile in $LOG_FILES; do
	if [ -n "$logfile" ]; then
		if [ -f "$LOG_FOLDER/$logfile" ]; then
			rm -v "$LOG_FOLDER/$logfile"
			#echo "$LOG_FOLDER/$logfile"
		fi
	fi
done

psql -c "$(envsubst < ~/semi-nuke.sql)"

crontab ~/.crontab.bak && rm ~/.crontab.bak
