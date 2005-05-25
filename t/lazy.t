use Test::More 'no_plan';
use Contextual::Return;

my $count_lazy  = 1;
my $count_eager = 1;

sub ID_lazy {
    return SCALAR { $count_lazy++ }
}

sub ID_eager {
    my $count = $count_eager++;
    return SCALAR { $count }
}

my $id_lazy  = ID_lazy();
my $id_eager = ID_eager();

for (1..5) {
    is $id_lazy,  $count_lazy              => "Lazy $count_lazy";
    is $id_eager, 1                        => "Eager";
}
