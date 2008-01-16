#!/usr/bin/perl -w
use strict;

use constant BET_STATE_CURRENT => 1;
use constant BET_STATE_SUCCESS => 2;
use constant BET_STATE_FAILURE => 3;
use Data::Dumper;

Utility::SQL->init;

printlog('Running Nightly Maintenance');
expire_bets();
printlog('Finished Nightly Maintenance');

sub expire_bets {
  my $bets = get_expired_bets();
  my $total = 0;
  foreach my $bet (@$bets) {
    expire_bet($bet);
    $total+= $bet->{price};
  }
  $total = sprintf "%.02f", ($total/100);
  my $bet_string = @$bets == 1 ? 'bet' : 'bets'; 
  printlog('Expired ' . @$bets . " $bet_string, valued at: \$$total");
}

sub expire_bet {
  my ($bet) = @_;
  my $descr = $bet->{descr};
  $descr =~ s/\n/ /g;
  $descr =~ s/:/./g;

  printlog("Expiring Bet: id:$bet->{id}, user_id:$bet->{user_id}, " .
    "due_date:$bet->{due_date}, descr:$descr");

  Utility::SQL->execute('update bets set state=? where id=?',
    BET_STATE_FAILURE, $bet->{id});
}

sub printlog {
  my ($msg) = @_;
  my $environment = Utility::SQL->environment;
  my $logfile = "/var/log/bom_$environment.log";
  open OUT, '>>', $logfile or die "Can't open $logfile\n";
  print OUT scalar localtime() . ": " . $msg . "\n";
  close OUT;
}

sub get_expired_bets {
  return Utility::SQL->rows(
    'select * from bets where due_date<current_date and state=?',
    BET_STATE_CURRENT,
  );
}

package Utility::SQL;
use DBI;
use DBD::Pg;
use Data::Dumper;

our ($dbh, $environment);

sub dbh { $dbh }
sub environment { $environment }

sub init {
  my ($class, $environment_arg) = @_;
  $environment ||= $environment_arg || $ENV{RAILS_ENV} || 'development';
  $dbh = DBI->connect("dbi:Pg:db=betonmyself_$environment", 'root', 'root');
}
sub rows {
  my ($class, $statement, @params) = @_;
  my $sth = $class->execute($statement, @params);
  return $sth->fetchall_arrayref({});
}
sub execute {
  my ($class, $statement, @params) = @_;
  my $sth = $dbh->prepare($statement);
  $sth->execute(@params);
  return $sth;
}
