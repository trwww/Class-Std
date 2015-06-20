use warnings;
use strict;

package Test ;
use base qw( Exporter ) ;

use Carp ;
use Class::Std ;

sub exec_proc {
  croak 'Test exception' ;
}	

sub DEMOLISH {
  eval { print "Demolish\n" }
  # without eval the exception in exec_proc gets forwarded to the caller
#  print "Demolish\n" ;
} 

package main;
use English;
use Test::More tests => 1;

my $subref =sub  {
  my $p_obj = Test->new( { } ) ;
  $p_obj->exec_proc() ; 
} ;

eval { $subref->() ;} ;

ok( $EVAL_ERROR, 'exception was not gobbled' );
