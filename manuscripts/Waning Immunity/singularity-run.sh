#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <image.sif> <command> [args...]"
    exit 1
fi

IMAGE="$1"
shift

# Load Singularity
module load singularity/4.2.0

# Prefer /scratch, fall back to /tmp
if [ -w /scratch ]; then
    TMP_BASE="/scratch/${USER}-singularity"
else
    TMP_BASE="/tmp/${USER}-singularity"
fi

mkdir -p "$TMP_BASE"

export SINGULARITY_TMPDIR="$TMP_BASE"
export TMPDIR="$TMP_BASE"

echo "SINGULARITY_TMPDIR=$SINGULARITY_TMPDIR"
echo "Running: singularity exec --userns $IMAGE $*"

exec singularity exec --userns "$IMAGE" "$@"
