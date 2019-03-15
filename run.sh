#!/usr/bin/env bash

THIS_DIR="$(dirname "$(readlink -f "$0")")"
cd "${THIS_DIR}"

read -r -d '' helpstring <<-EOF
	Usage: run.sh <mode> <infile>

	mode: scan|tokens|parse|pretty|typecheck
	infile: name of file to process
EOF

mode="${1}"
infile="${2}"

# check for correct argument
if [[ -z "${mode}" || ! "${mode}" =~ ^scan|tokens|parse|pretty|typecheck$ ]]; then
    echo "ERROR: first argument <mode> must match ^scan|tokens|parse|pretty|typecheck$"
    echo ""
    echo "$helpstring"
    exit 1
fi

# check that infile is a file
if [ ! -f "${infile}" ]; then
    echo "ERROR: second argument <infile> does not exist"
    echo ""
    echo "$helpstring"
    exit 1
fi

# check that "build.sh" has been ran
if [ "$(ls -1 "src" | grep "native")" == "" ]; then
    echo "ERROR: run build.sh before run.sh"
    exit 1
fi

case "${mode}" in
    "scan")
        ./src/main_scan.native < "${infile}"
        ;;
    "tokens")
        ./src/main_tokens.native < "${infile}"
        ;;
    "parse")
        ./src/main_parse.native < "${infile}"
        ;;
    "pretty")
        ./src/main_pretty.native < "${infile}"
        ;;
    "typecheck")
        ./src/main_typecheck.native < "${infile}"
        ;;
esac
