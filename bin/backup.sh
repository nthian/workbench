#!/bin/sh

DEFAULT_FROM_DIR=${HOME}
DEFAULT_TO_DIR=/media/${USER}/data/home

ARG_NUM=0
EXTRA_ARGS=""
EXTRA_SEP=""
for arg in $@
do
    case $arg in
	--restore_from_backup)
	    BACKUP_FROM_DIR=$DEFAULT_TO_DIR
	    BACKUP_TO_DIR=$DEFAULT_FROM_DIR
	    ;;
         --*) 
            case $arg in
                --delete)
                    EXTRA_ARGS="${EXTRA_ARGS}${EXTRA_SEP}${arg}"
                    ;;
                *)
                    echo "WARNING: Unsupported flag ${arg} discarded." >&2
                    ;;
            esac
            EXTRA_SEP=" "
            ;;
       *)
            ARG_NUM=$((${ARG_NUM}+1))
            if [ -e ${arg} ]; then
                if [ $ARG_NUM -eq 1 ]; then
                    BACKUP_FROM_DIR="${arg}"
                elif [ $ARG_NUM -eq 2 ]; then
                    BACKUP_TO_DIR="${arg}"
                else
                    echo "ERROR: Extraneous argument provided: ${arg}" >&2
                fi
            fi
    esac
done

if [ $ARG_NUM -gt 2 ]; then
    echo "ERROR: Too many arguments provided." >&2
    exit 1
fi

BACKUP_FROM_DIR=${BACKUP_FROM_DIR:-${DEFAULT_FROM_DIR}}
BACKUP_TO_DIR=${BACKUP_TO_DIR:-${DEFAULT_TO_DIR}}
if [ -d ${BACKUP_TO_DIR} ]; then
    rsync -avp ${EXTRA_ARGS} ${BACKUP_FROM_DIR}/ ${BACKUP_TO_DIR}/ 2>&1
fi
