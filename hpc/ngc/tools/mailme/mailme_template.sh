#!/bin/bash

# Try getting username from passwd
USER_NAME="$(getent passwd "$USER" | cut -d ":" -f 5 | cut -d " " -f 1)"
if [[ "$USER_NAME" == "" ]]; then
	USER_NAME="$USER"
fi

echo "Hello, $USER_NAME!

Attached are the file(s) requested from the NGC HPC.

Current time and date is $(date +"%d/%m %H:%M:%S")"
