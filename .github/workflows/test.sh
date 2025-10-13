inputs.schemas="test"

echo "ALTER SESSION SET CONTAINER = FREEPDB1;" > setup.sql

# Create schemas from input
IFS=',' read -ra schemas <<< "${{ inputs.schemas }}"
for schema in "${schemas[@]}"; do
# Create user/schema
echo "CREATE USER \'${schema}\' IDENTIFIED BY \'${schema}_pwd\';" >> setup.sql

# Grant privileges
echo "GRANT CREATE SESSION, CREATE TABLE, UNLIMITED TABLESPACE TO \"${schema}\";" >> setup.sql
done