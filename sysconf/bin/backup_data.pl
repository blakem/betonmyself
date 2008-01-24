#!/usr/bin/perl -w
use strict;
use Getopt::Std;
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
}
