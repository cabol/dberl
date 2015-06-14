#!/bin/sh
#
# ----------------------------------------------------------------------
# Couchbase cleanup script.
# ----------------------------------------------------------------------
#

READLINK=`which readlink`
DIRNAME=`which dirname`

# ----------------------------------------------------------------------
# Ensure SCRIPT_HOME points to the correct directory.
# ----------------------------------------------------------------------

SCRIPT_LOCATION=$0
if [ -x "$READLINK" ]; then
  while [ -L "$SCRIPT_LOCATION" ]; do
    SCRIPT_LOCATION=`"$READLINK" -e "$SCRIPT_LOCATION"`
  done
fi
SCRIPT_HOME="$( cd "$( dirname "$SCRIPT_LOCATION" )" && cd .. && pwd )"
echo "SCRIPT_HOME: '$SCRIPT_HOME'"

# ----------------------------------------------------------------------
# Get connection parameters.
# ----------------------------------------------------------------------

echo "---------- COUCHBASE CONNECTION ----------"

read -p "> Enter Couchbase Host [127.0.0.1]: " cb_host
if [ "$cb_host" == "" ]; then
  cb_host=127.0.0.1
fi
read -p "> Enter Couchbase Admin Port [8091]: " cb_admin_port
if [ "$cb_admin_port" == "" ]; then
  cb_admin_port=8091
fi
read -p "> Enter Couchbase username []: " cb_username
read -p "> Enter Couchbase password []: " cb_password
if [ "$cb_username" != "" ] && [ "$cb_password" != "" ]; then
  cb_auth=$cb_username:$cb_password@
else
  cb_auth=""
fi
BASE_ADMIN_URL=http://$cb_auth$cb_host:$cb_admin_port;
echo "\nBASE ADMIN URL: $BASE_ADMIN_URL"

# ----------------------------------------------------------------------
# Create System Buckets and Views.
# ----------------------------------------------------------------------

echo "---------- SYSTEM BUCKETS ----------"

while true; do
  read -p "Do you want to delete a bucket?" yn
  case $yn in
    [Yy]* )
      # Get bucket name
      read -p "> Bucket Name [defaut]: " bucketName
      if [ "$bucketName" == "" ]; then
        bucketName=defaut
      fi
      # Delete the bucket
      curl -vX DELETE $BASE_ADMIN_URL/pools/default/buckets/$bucketName
      echo "\nBucket $bucketName deleted.\n";;
    [Nn]* ) break;;
    * ) echo "Please answer yes or no.";;
  esac
done

echo END.
