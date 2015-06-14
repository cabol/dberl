#!/bin/sh
#
# ----------------------------------------------------------------------
# Couchbase setup script.
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
read -p "> Enter Couchbase API Port [8092]: " cb_api_port
if [ "$cb_api_port" == "" ]; then
  cb_api_port=8092
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
BASE_API_URL=http://$cb_auth$cb_host:$cb_api_port;
echo "BASE API URL: $BASE_API_URL\n"

# ----------------------------------------------------------------------
# Create System Buckets and Views.
# ----------------------------------------------------------------------

echo "---------- SYSTEM BUCKETS ----------"

while true; do
  read -p "Do you want to create a new bucket?" yn
  case $yn in
    [Yy]* )
      # Get bucket properties
      read -p "> Bucket Name [defaut]: " bucketName
      if [ "$bucketName" == "" ]; then
        bucketName=defaut
      fi
      read -p "> Per Node RAM Quota [100] MB: " ramQuotaMB
      if [ "$ramQuotaMB" == "" ]; then
        ramQuotaMB=100
      fi
      read -p "> Authentication Type [sasl]: " authType
      if [ "$authType" == "" ]; then
        authType=sasl
      fi
      read -p "> Replica Number [1]: " replicaNumber
      if [ "$replicaNumber" == "" ]; then
        replicaNumber=1
      fi
      read -p "> Proxy Port []: " proxyPort
      if [ "$proxyPort" == "" ]; then
        proxyPort=""
      fi
      read -p "> Flush Enabled [0]: " flushEnabled
      if [ "$flushEnabled" == "" ]; then
        flushEnabled=0
      fi
      # Create the bucket
      curl -vX POST -d name=$bucketName -d ramQuotaMB=$ramQuotaMB \
        -d authType=$authType -d replicaNumber=$replicaNumber \
        -d proxyPort=$proxyPort -d flushEnabled=$flushEnabled \
        $BASE_ADMIN_URL/pools/default/buckets
      echo "\nBucket $bucketName created.\n"
      # Create Views
      while true; do
        read -p "Do you want to create views?" yn
        case $yn in
          [Yy]* )
            read -p "> View's Path [$SCRIPT_HOME/views]: " viewsPath
            if [ "$viewsPath" == "" ]; then
              viewsPath=$SCRIPT_HOME/views
            fi
            read -p "> Enter design document name: " designDoc
            curl -vX PUT -H 'Content-Type: application/json' \
              $BASE_API_URL/$bucketName/_design/$designDoc \
              -d @"$viewsPath/$designDoc.json"
              echo "Design doc. $designDoc created.\n";;
          [Nn]* ) break;;
        esac
      done;;
    [Nn]* ) break;;
    * ) echo "Please answer yes or no.";;
  esac
done

echo END.
