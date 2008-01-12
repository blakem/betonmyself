if [ -f "$root/tmp/pids/mongrel.pid" ]; then
  (cd $root; mongrel_rails stop -P $root/tmp/pids/mongrel.pid)
else
  echo "Can't find mongrel pid: $root/tmp/pids/mongrel.pid"
fi

