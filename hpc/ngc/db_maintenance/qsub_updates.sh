#!/bin/bash

# qsub_updates.sh
#
# A crontab-friendly script

usage() (
	cat <<EOM
Usage:
$(basename $0) -t target_table -s target_script source_table [source_table ...]

Optional parameters:
  -v	verbose mode
  -h	display this help message and exit
  -n	dry run; do not send jobs to queue, do not run parse_logs.R after queueing
  -m  sets the mode. supply "qsub" to use PBS on the fatnode. supply "screen" to use screens on the fat node
  -d  use "date" rather than "end_time" to look for new jobs
  -a  any update to source tables trigger new runs for target_script
  -f  full mode; sets the environment variable "FULL_MODE" in the first iteration of timestamps
EOM
)

sql_to_lines(){
	# Execute an SQL query and strip leading whitespace
	psql -h 172.18.17.103 data -tc "$1" | grep -o "[^[:space:]].*"
}

send_to_queue(){
	# If this script has previously queued any jobs, wait until they exit
	if [ ! -z ${QUEUED_JOB+x} ]; then
		QUEUED_JOB="-W depend=afterok:$QUEUED_JOB"
	fi

	QUEUED_JOB=$(qsub \
	  -N $TARGET_BASENAME \
	  -A ssi_mg \
	  -W group_list=ssi_mg \
	  -v TIMESTAMP="$TIMESTAMP",TARGET_TABLE="$TARGET_TABLE",SOURCE_TABLES="$SOURCE_TABLES" \
	  -o "$TARGET_BASENAME"_$(date -d $TIMESTAMP +%y%m%d).log \
	  -e "$TARGET_BASENAME"_$(date -d $TIMESTAMP +%y%m%d).err \
	  $QUEUED_JOB \
	  $TARGET_SCRIPT $FULL_MODE) || exit 1

	echo $QUEUED_JOB
}

send_to_screen(){
	[ ! $DRY_RUN ] &&\
	screen -S "$TARGET_BASENAME" -X stuff "$@"
}

queue_to_screen(){
	# Exit if screen with target basename is already running
	ls /var/run/screen/S-$USER/*.$TARGET_BASENAME >/dev/null 2>&1 &&\
	{ echo "There is already a screen running for $TARGET_BASENAME" >&2
	exit 1; }

	# Read -d flag from PBS directive; set to HOME if not found
	PWD=$(grep -Po "(?<=^#PBS -d ).+" $TARGET_SCRIPT) || PWD=$HOME
	echo PWD=$PWD

	# Initialize screen
	[ ! $DRY_RUN ] && {
		screen -dmS "$TARGET_BASENAME"

		TMPFILE="$(mktemp /tmp/$USER.$TARGET_BASENAME.XXX)"
		echo TMPFILE=$TMPFILE
		cp -p "$TARGET_SCRIPT" "$TMPFILE"
	}

	# Screen has issues running R unless modules are reloaded
	if [ "${VERBOSITY-0}" -eq 0 ]; then
		send_to_screen "stty -echo
"
	fi
	send_to_screen "module reload >/dev/null 2>&1 && cd ${PWD-$HOME}; clear
"

	# Construct loop to execute in screen
	for ts in $@; do
		echo "$ts"
		send_to_screen "(echo TIMESTAMP="'"'$ts'"'" $TMPFILE; "
		send_to_screen "TIMESTAMP=\"$ts\" $TMPFILE $FULL_MODE 2>&1 | tee -a "
		send_to_screen "$PWD/$TARGET_BASENAME"
		send_to_screen "$(date -d "$ts" +_%m%d).log) && "
		FULL_MODE= # Ensure FULL_MODE is unset after first iteration
	done

	# Remove copy of target script from /tmp
	send_to_screen "true; rm $TMPFILE && exit
"
}

# Only echo if -v is given
echo(){ if [ ${VERBOSITY-0} -gt 0 ]; then builtin echo "$@"; fi }

# Show usage if no arguments are given
if [ $# -eq 0 ]; then
	usage
	exit 0
fi

# Parse arguments
while getopts "t:s:hnm:vdaf" arg; do
	case ${arg} in
		t)
			TARGET_TABLE+=" ${OPTARG}"
			;;
		s)
			TARGET_SCRIPT=${OPTARG}
			TARGET_BASENAME=$(basename $TARGET_SCRIPT | sed "s/[.][^.]*//g")
			;;
		m)
			MODE="${OPTARG}"
			;;
		n)
			DRY_RUN=true
			;;
		v)
			VERBOSITY=1
			;;
		d)
		 	USE_DATE=true
			;;
		a)
			RUN_AT_ANY_UPDATE=true
			;;
		f)
		  FULL_MODE="--full"
		  ;;
		h)
			usage
			exit 0
			;;
	esac
done
shift $((OPTIND-1))

SOURCE_TABLES=$@
TARGET_TABLE=`cut -c2- <<< "$TARGET_TABLE"`

# Exit if no target table or script is given
if [ -z $TARGET_TABLE ] || [ -z $TARGET_SCRIPT ]; then
	usage
	exit 1
fi

echo MODE=$MODE
echo TARGET_TABLE=$TARGET_TABLE
echo TARGET_SCRIPT=$TARGET_SCRIPT
echo TARGET_BASENAME=$TARGET_BASENAME
echo SOURCE_TABLES=$SOURCE_TABLES

# Exit if any jobs matching TARGET_BASENAME are already running
get_qstat_values (){
	qstat -f | while read line; do
		if [[ "$line" == "Job Id: "* ]]; then  # Reset outputs
			line=$(sed "s/Job Id: /Job_Id = /" <<< "$line")

			declare -A OUTPUT
		fi

		IFS=' = ' read KEY VALUE <<< "$line"

		if [[ "$@" == *"$KEY"* ]] && [ -n "$KEY" ]; then
			OUTPUT[$KEY]="$VALUE"
		fi

		# When each qstat returns a newline, print values in given order
		if [ ! -n "$line" ]; then
			for KEY in $@; do
				echo -n "${OUTPUT[$KEY]} "
				OUTPUT[$KEY]=""
			done

			# Finally, print a newline
			echo
		fi
	done
}

get_qstat_values \
  Job_Id \
  Job_Name \
  job_state \
  Resource_List.walltime \
  resources_used.walltime \
  exit_status |\
while read line; do
	read job_id job_name job_state walltime elapsed exit_status <<< "$line"

	if [ "$job_name" == "$TARGET_BASENAME" ] && [ "$job_state" != "C" ]; then
		echo -n "Job is already running! ($job_id, state $job_state" >&2
		if [ -n "$elapsed" ]; then
			echo -n ", $elapsed/$walltime"
		fi
		echo ")"

		exit 1  # Exiting from here doesn't exit the script; therefore exit 0 to catch this
	fi
done

if [ $? -ne 0 ] && [ ! $DRY_RUN ]; then
	exit 0
fi


# Set script to filter by "date" rather than "end_time" if -d mode is used
# (Useful for running jobs for all missing dates during development)
if [ $USE_DATE ]; then
	DATE_ENDTIME='date'
else
	DATE_ENDTIME='end_time'
fi
echo DATE_ENDTIME=$DATE_ENDTIME


# Set how many source tables are required to be updated before target script is called
if [ $RUN_AT_ANY_UPDATE ]; then
	REQUIRED_UPDATES=1
else
	REQUIRED_UPDATES=$(wc -w <<< $SOURCE_TABLES)
fi
echo REQUIRED_UPDATES=$REQUIRED_UPDATES

if [ ! -z $FULL_MODE ]; then echo FULL_MODE=true; fi


# If source tables: check if any updates (from logs)
module is-loaded postgresql || module load postgresql/15.1
if ! pg_isready -h 172.18.17.103 -d data >/dev/null 2>&1; then
	echo "Postgresql is not running" >&2
	exit 1
fi
LAST_UPDATE=$(sql_to_lines "
SELECT
	$DATE_ENDTIME
FROM (
	SELECT
		MAX($DATE_ENDTIME) AS date, max(end_time) AS end_time
	FROM prod.logs
	WHERE CONCAT_WS('.', COALESCE(logs.schema, 'prod'), logs.table) = ""'""$TARGET_TABLE""'""
  	AND success
) q01"
)


# If TARGET_TABLE is not in prod.logs, look in TARGET_TABLE itself
if [ ! -n "$LAST_UPDATE" ]; then
	LAST_UPDATE=$(sql_to_lines "
SELECT
	MAX(ts) $DATE_ENDTIME
FROM (
SELECT MAX(from_ts) ts FROM $TARGET_TABLE
UNION
SELECT MAX(until_ts) ts FROM $TARGET_TABLE) q01" 2>/dev/null)
fi

echo LAST_UPDATE=$LAST_UPDATE

if [ ! -n "$LAST_UPDATE" ]; then
	LAST_UPDATE="1900-01-01"
fi

# Get list of timestamps
TIMESTAMPS="$(sql_to_lines "
SELECT DISTINCT
  MAX(date) OVER (PARTITION BY date::DATE) date
FROM (
  SELECT
	  *,
	  SUM(first_success) OVER (PARTITION BY date::DATE) csum  -- Tables with >= success per date
  FROM (
    SELECT
  	  date,
  	  logs.table,
	  end_time,
	  CASE
	    WHEN ROW_NUMBER() OVER (PARTITION BY date::DATE, logs.table ORDER BY end_time) = 1 THEN 1
	    ELSE 0
	  END as first_success
    FROM
	  prod.logs
    WHERE success
      AND CONCAT_WS('.', COALESCE(logs.schema, 'prod'), logs.table) IN ($(printf ", '%s'" $SOURCE_TABLES | cut -c 3-))
      AND $DATE_ENDTIME > ""'""$LAST_UPDATE""'""
      -- AND (${RUN_AT_ANY_UPDATE-FALSE} OR (n_insertions > 0 OR n_deactivations > 0))
  ) q01
) q02
WHERE csum >= $REQUIRED_UPDATES
ORDER BY date
")"

echo TIMESTAMPS=$TIMESTAMPS

IFS=$'\n'

if [ "$MODE" == "qsub" ]; then
	for TIMESTAMP in $TIMESTAMPS; do
		echo $TIMESTAMP
		[ ! $DRY_RUN ] && send_to_queue
		FULL_MODE= # Ensure FULL_MODE is unset after first iteration
	done
elif [ "$MODE" == "screen" ]; then
	queue_to_screen $TIMESTAMPS
fi

if [ ! $DRY_RUN ] && [ ! -z ${TIMESTAMPS+x} ]; then
	/ngc/projects/ssi_mg/common/db_maintenance/parse_logs.R
fi
