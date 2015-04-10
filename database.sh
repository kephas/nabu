#!/bin/bash

if [[ -n "$1" ]]; then
    ACTION=$1;
else
    echo "Usage: ./database.sh backup|restore [version]";
    exit 1;
fi

if [[ -n "$2" ]]; then
    VERSION="-$2";
else
    VERSION="";
fi;

DIR=/root/backup;
FILE=nabu-backup${VERSION}.sql;

PASSWORD=nabu;
USERNAME=nabu;
DATABASE=nabu;

CONTAINER=nabu_pg_1;

case $ACTION in
    backup)
	EXEC=pg_dump;;
    restore)
	EXEC=psql;;
esac;

COMMAND="${EXEC} --host=\$PG_PORT_5432_TCP_ADDR --username=${USERNAME} --dbname=${DATABASE} --file=${DIR}/${FILE}"

docker run -e PGPASSFILE=$DIR/pgpass -v $PWD/backup:$DIR --link $CONTAINER:pg -ti postgres bash -c "${COMMAND}"
