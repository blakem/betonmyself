package Utility::SQL;
use strict;
use DBI;
use DBD::Pg;
use Data::Dumper;

our ($dbh, $environment);

sub dbh { $dbh }
sub environment { $environment }

sub init {
  my ($class, $environment_arg) = @_;
  $environment ||= $environment_arg || $ENV{RAILS_ENV} || 'development';
  $dbh->disconnect if $dbh;
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
sub select {
  my ($class, $statement, @params) = @_;
  my $sth = $class->execute($statement, @params);
  return @{$sth->fetchall_arrayref({})};
}
sub list {
  my ($class, $statement, @params) = @_;
  my $sth = $class->execute($statement, @params);
  return map { $_->[0] } @{$sth->fetchall_arrayref};
}

1;
