#!/usr/bin/env bash

full_nuke() {
	echo "find /ngc/projects/ssi_mg/logs -name '*_$SCHEMA.$TABLE.log' -exec rm {} \;"
	echo "psql -c \"DELETE FROM prod.logs WHERE logs.table = '$TABLE' AND logs.schema = '$SCHEMA'\""
	echo "psql -c \"DELETE FROM docs.documentation WHERE documentation.table = '$TABLE' AND documentation.schema = '$SCHEMA'\""
	echo "psql -c \"DROP TABLE $SCHEMA.$TABLE\""
}

tactical_nuke() {
	local SCHEMA=$1
	local TABLE=$2
	local CUTOFF=$(envsubst <<< "SELECT min(date) FROM prod.logs WHERE COALESCE(\"schema\", 'prod') = '$SCHEMA' AND \"table\" = '$TABLE'" | psql -tA)

	# Ensure that table is versioned
	local result=$(printf "
		SELECT
			COUNT(*)
		FROM information_schema.columns
		WHERE table_schema = '$SCHEMA' AND table_name = '$TABLE'
		  AND column_name IN ('checksum', 'from_ts', 'until_ts')" | psql -tA)

	if [ $result -ne 3 ]; then
		echo "Table $SCHEMA.$TABLE does not seem to be a versioned table" >&2
		exit 1
	fi

	# Identify log files to delete
	TABLE=$TABLE SCHEMA=$SCHEMA CUTOFF=$CUTOFF envsubst <<< \
'psql -c "DELETE FROM \"$SCHEMA\".\"$TABLE\" WHERE from_ts > '\''$CUTOFF'\''"
psql -c "UPDATE \"$SCHEMA\".\"$TABLE\" SET until_ts = NULL WHERE until_ts >= '\''$CUTOFF'\''"
psql -c "DELETE FROM docs.documentation WHERE \"schema\" = '\''$SCHEMA'\'' AND \"table\" = '\''$TABLE'\'' AND from_ts > '\''$CUTOFF'\''"
psql -c "UPDATE docs.documentation SET until_ts = NULL WHERE until_ts >= '\''$CUTOFF'\''"
psql -tAc "SELECT log_file FROM prod.logs WHERE \"schema\" = '\''$SCHEMA'\'' AND \"table\" = '\''$TABLE'\''" | xargs -i rm "/ngc/projects/ssi_mg/logs/{}"'

	#printf "psql -c 'UPDATE \"%s\".\"%s\" SET until_ts = NULL'\n" "$SCHEMA" "$TABLE"
	#printf "psql -c 'DELETE FROM prod.logs WHERE COALESCE(\"schema\", 'prod') = '$SCHEMA' AND \"table\" = '$TABLE'\n"
}

if [ "$1" == "--tactical" ]; then
	TACTICAL=true
	shift
fi

IFS='.' read -r SCHEMA TABLE <<< "$1"

if [ -z ${TABLE} ]; then
	echo "Full specification not given. Must be on form: schema.table" >&2
	exit 1
fi

# Exit if table does not exist
module is-loaded postgresql/15.1 || module load postgresql/15.1
MATCHES=$(envsubst <<< "SELECT COUNT(*) FROM pg_tables WHERE schemaname='$SCHEMA' AND tablename='$TABLE'" | psql -tA)

if [ $MATCHES -ne 1 ]; then
	echo "Could not find table $SCHEMA.$TABLE" >&2
	exit 1
fi

if [ ${TACTICAL} ]; then
	tactical_nuke $SCHEMA $TABLE
else
	full_nuke
fi
