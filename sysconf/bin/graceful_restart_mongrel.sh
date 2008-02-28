if [ "$1" = "" ]; then
  pidfile=$root/tmp/pids/mongrel.pid
else
  pidfile=$1
fi

echo "Restarting mongrel from: $pidfile"
mongrel_rails mongrel::restart -P $pidfile
