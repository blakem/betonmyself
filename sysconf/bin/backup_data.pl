#!/usr/bin/perl -w
use strict;

use Getopt::Std;
use Utility::SQL;

my %opts;
getopts('r', \%opts);

my @environments = qw(development production);

foreach my $environment (@environments){
  if ($opts{r}) {
    restore_data($environment);
  } else {
    backup_data($environment);
  }
}

sub backup_data {
  my ($env) = @_;
  print "Backing up betonmyself_$env\n";
  system("pg_dump --data-only -Fc -f /tmp/bom_$env.tar betonmyself_$env");
}

sub restore_data {
  my ($env) = @_;
  print "Restoring betonmyself_$env\n";
  Utility::SQL->init($env);

  my @lines = split /\n/, `pg_restore -l /tmp/bom_$env.tar`;
  my ($user_line) = grep { /TABLE DATA public users/ } @lines;
  open OUT, ">/tmp/bom_$env.list" or die;
  my $user_line_printed;
  foreach (@lines) {
    next if /TABLE DATA public users/;
    if (not $user_line_printed and /TABLE DATA public/) {
      print OUT "$user_line\n";
      $user_line_printed = 1;
    }
    print OUT "$_\n";
  }
  close OUT;
  system("pg_restore -d betonmyself_$env -L /tmp/bom_$env.list /tmp/bom_$env.tar");
  my @primary_keys = Utility::SQL->list("
    select conname from pg_constraint where conname like '%_pkey'
  ");
  foreach my $pkey (@primary_keys) {
    my $table = $pkey;
    $table =~ s/_pkey$//;
    my $seq = "${table}_id_seq";
    Utility::SQL->execute("
      select setval('$seq', (select max(id) from $table))
    ");
  }
}
