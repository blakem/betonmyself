if [ "$1" = "" ]; then
  environment='development'
  port=8080
else
  environment=$1
  port=8000
fi

if [ -f "$root/tmp/pids/mongrel.pid" ]; then
  (cd $root; sysconf/bin/stop_mongrel.sh)
  sleep 3;
fi
(cd $root; mongrel_rails start -d -p $port -e $environment -P $root/tmp/pids/mongrel.pid -a 127.0.0.1)

