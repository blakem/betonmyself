if [ "$1" = "" ]; then
  environment='development'
else
  environment=$1
fi
if [ -f "$root/tmp/pids/mongrel.pid" ]; then
  (cd $root; sysconf/bin/stop_mongrel.sh)
  sleep 3;
fi
(cd $root; mongrel_rails start -d -p 8000 -e $environment -P $root/tmp/pids/mongrel.pid -a 127.0.0.1)

