package Class::Std;

use version; $VERSION = qv('0.0.2');
use warnings;
use Carp;
use Scalar::Util;

use overload;

*ID = \&Scalar::Util::refaddr;

my (%attribute, %cumulative, %anticumulative);

my @exported_subs = qw(
    new
    DESTROY
    MODIFY_HASH_ATTRIBUTES
    MODIFY_CODE_ATTRIBUTES
    AUTOLOAD
    _DUMP
);

sub import {
    my $caller = caller;

    no strict 'refs';
    *{ $caller . '::ident'   } = \&Scalar::Util::refaddr;
    for my $sub ( @exported_subs ) {
        *{ $caller . '::' . $sub } = \&{$sub};
    }
}

sub _find_sub {
    my ($package, $sub_ref) = @_;
    for my $name (keys %{$package.'::'}) {
        my $candidate = *{$package.'::'.$name}{CODE};
        return $name if $candidate && $candidate == $sub_ref;
    }
    croak q{Can't make anonymous subroutine cumulative};
}

sub _str {
    my ($pat) = @_;
    return qr{ '($pat)' | "($pat)"
             | qq? (?:
                     /($pat)/ | \{($pat)\} | \(($pat)\) | \[($pat)\] | <($pat)>
                   )
             }xms;
}

my $STR = _str( qr{(.*?)} );
my $NUM = qr{ ( [-+]? (?:\d+\.?\d*|\.\d+) (?:[eE]\d+)? ) }xms;

sub _extractor_for_pair_named {
    my ($key) = @_;

    $key = qr{\Q$key\E};
    $str_key = _str($key);

    my $matcher = qr{ :$key<  \s* ([^>]*) \s* >
                    | :$key«  \s* ([^»]*) \s* »
                    | :$key\( \s*  (?:$STR | $NUM)   \s* \)
                    | (?: $key | $str_key ) \s* => \s* (?: $STR | $NUM )
                    }xms;

    return sub { return $_[0] =~ $matcher ? $+ : undef };
}

*_extract_default  = _extractor_for_pair_named('default');
*_extract_init_arg = _extractor_for_pair_named('init_arg');
*_extract_get      = _extractor_for_pair_named('get');
*_extract_set      = _extractor_for_pair_named('set');

sub MODIFY_HASH_ATTRIBUTES {
    my ($package, $referent, @attrs) = @_;
    for my $attr (@attrs) {
        next if $attr !~ m/\A ATTRS? \s* (?:[(] (.*) [)] )? \z/xms;
        my ($default, $init_arg, $getter, $setter);
        if (my $config = $1) {
            $default  = _extract_default($config);
            $init_arg = _extract_init_arg($config);

            if ($getter = _extract_get($config)) {
                *{$package.'::get_'.$getter} = sub {
                    return $referent->{ID($_[0])};
                }
            }
            if ($setter = _extract_set($config)) {
                *{$package.'::set_'.$setter} = sub {
                    croak "Missing new value in call to 'set_$setter' method"
                        unless @_ == 2;
                    my ($self, $new_val) = @_;
                    my $old_val = $referent->{ID($self)};
                    $referent->{ID($self)} = $new_val;
                    return $old_val;
                }
            }
        }
        undef $attr;
        push @{$attribute{$package}}, {
            ref      => $referent,
            default  => $default,
            init_arg => $init_arg,
            name     => $init_arg || $getter || $setter || '????',
        };
    }
    return grep {defined} @attrs;
}

sub _DUMP {
    my ($self) = @_;
    my $id = ID($self);

    my %dump;
    for my $package (keys %attribute) { 
        my $attr_list_ref = $attribute{$package};
        for my $attr_ref ( @{$attr_list_ref} ) {
            next if !exists $attr_ref->{ref}{$id};
            $dump{$package}{$attr_ref->{name}} = $attr_ref->{ref}{$id};
        }
    }

    use Data::Dumper 'Dumper';
    my $dump = Dumper \%dump;
    $dump =~ s/^.{8}//gxms;
    return $dump;
}

my $STD_OVERLOADER
    = q{ package %%s;
         use overload (
            q{%s} => sub { $_[0]->$referent(ident $_[0]) },
            fallback => 1
         );
       };

my %OVERLOADER_FOR = (
    STRINGIFY => sprintf( $STD_OVERLOADER, q{""}   ),
    NUMERIFY  => sprintf( $STD_OVERLOADER, q{0+}   ),
    BOOLIFY   => sprintf( $STD_OVERLOADER, q{bool} ),
    SCALARIFY => sprintf( $STD_OVERLOADER, q{${}}  ),
    ARRAYIFY  => sprintf( $STD_OVERLOADER, q{@{}}  ),
    HASHIFY   => sprintf( $STD_OVERLOADER, q{%%{}} ),  # %% to survive sprintf
    GLOBIFY   => sprintf( $STD_OVERLOADER, q{*{}}  ),
    CODIFY    => sprintf( $STD_OVERLOADER, q{&{}}  ),
);

use List::Util qw( first );
use Scalar::Util;

sub MODIFY_CODE_ATTRIBUTES {
    my ($package, $referent, @attrs) = @_;
    for my $attr (@attrs) {
        if ($attr eq 'CUMULATIVE') {
            undef $attr;
            push @{$cumulative{$package}}, $referent;
        }
        elsif ($attr =~ m/\A CUMULATIVE \s* [(] \s* BASE \s* FIRST \s* [)] \z/xms) {
            undef $attr;
            push @{$anticumulative{$package}}, $referent;
        }
        elsif (first {$attr eq $_} keys %OVERLOADER_FOR) {
            local $^W;
            eval sprintf $OVERLOADER_FOR{$attr}, ($package)x2;
            die "Internal error: $@" if $@;
            undef $attr;
        }
    }
    return grep {defined} @attrs;
}

my %_hierarchy_of;

sub _hierarchy_of {
    my ($class) = @_;

    return @{$_hierarchy_of{$class}} if exists $_hierarchy_of{$class};

    my @hierarchy = $class;
    my @parents   = @{$class.'::ISA'};

    while (defined (my $parent = shift @parents)) {
        push @hierarchy, $parent;
        push @parents, @{$parent.'::ISA'};
    }

    my %seen;
    return @{$_hierarchy_of{$class}}
        = sort { $a->isa($b) ? -1
               : $b->isa($a) ? +1
               :                0
               } grep !$seen{$_}++, @hierarchy;
}

my %_reverse_hierarchy_of;

sub _reverse_hierarchy_of {
    my ($class) = @_;

    return @{$_reverse_hierarchy_of{$class}}
        if exists $_reverse_hierarchy_of{$class};

    my @hierarchy = $class;
    my @parents   = reverse @{$class.'::ISA'};

    while (defined (my $parent = shift @parents)) {
        push @hierarchy, $parent;
        push @parents, reverse @{$parent.'::ISA'};
    }

    my %seen;
    return @{$_reverse_hierarchy_of{$class}}
        = reverse sort { $a->isa($b) ? -1
                       : $b->isa($a) ? +1
                       :                0
                       } grep !$seen{$_}++, @hierarchy;
}

CHECK {
    my (%cumulative_named, %anticumulative_named);

    for my $package (keys %cumulative) {
        for my $sub_ref (@{$cumulative{$package}}) {
            my $name = _find_sub($package, $sub_ref);
            $cumulative_named{$name}{$package} = $sub_ref;
            no warnings 'redefine';
            *{$package.'::'.$name} = sub {
                my $class = ref($_[0]) || $_[0];
                my $list_context = wantarray; 
                my (@result, @classes);
                for my $parent (_hierarchy_of($class)) {
                    my $sub_ref = $cumulative_named{$name}{$parent} or next;
                    ${$parent.'::AUTOLOAD'} = $AUTOLOAD if $name eq 'AUTOLOAD';
                    if (!defined $list_context) {
                        &{$sub_ref};
                        next;
                    }
                    push @classes, $parent;
                    if ($list_context) {
                        push @results, &{$sub_ref};
                    }
                    else {
                        push @results, scalar &{$sub_ref};
                    }
                }
                return if !defined $list_context;
                return @results if $list_context;
                return Class::Std::SCR->new({
                    values  => \@results,
                    classes => \@classes,
                });
            };
        }
    }

    for my $package (keys %anticumulative) {
        for my $sub_ref (@{$anticumulative{$package}}) {
            my $name = _find_sub($package, $sub_ref);
            if ($cumulative_named{$name}) {
                for my $other_package (keys %{$cumulative_named{$name}}) {
                    next unless $other_package->isa($package)
                             || $package->isa($other_package);
                    print {STDERR}
                        "Conflicting definitions for cumulative method",
                        " '$name'\n",
                        "(specified as :CUMULATIVE in class '$other_package'\n",
                        " but declared :CUMULATIVE(BASE FIRST) in class ",
                        " '$package')\n";
                    exit(1);
                }
            }
            $anticumulative_named{$name}{$package} = $sub_ref;
            no warnings 'redefine';
            *{$package.'::'.$name} = sub {
                my $class = ref($_[0]) || $_[0];
                my $list_context = wantarray; 
                my (@results, @classes);
                for my $parent (_reverse_hierarchy_of($class)) {
                    my $sub_ref = $anticumulative_named{$name}{$parent} or next;
                    if (!defined $list_context) {
                        &{$sub_ref};
                        next;
                    }
                    push @classes, $parent;
                    if ($list_context) {
                        push @results, &{$sub_ref};
                    }
                    else {
                        push @results, scalar &{$sub_ref};
                    }
                }
                return if !defined $list_context;
                return @results if $list_context;
                return Class::Std::SCR->new({
                    values  => \@results,
                    classes => \@classes,
                });
            };
        }
    }
}

sub new {
    my ($class, $arg_ref) = @_;

    croak "Can't find class $class" if ! keys %{$class.'::'};

    croak "Argument to $class->new() must be hash reference"
        if @_ > 1 && ref $arg_ref ne 'HASH';

    my $new_obj = bless \my($anon_scalar), $class;
    my $new_obj_id = ID($new_obj);
    my (@missing_inits, @suss_keys);

    $arg_ref ||= {};
    BUILD: for my $base_class (_reverse_hierarchy_of($class)) {
        my %arg_set = ( %{$arg_ref}, %{$arg_ref->{$base_class}||{}} );

        # Apply BUILD() methods...
        if (my $build_ref = *{$base_class.'::BUILD'}{CODE}) {
            $build_ref->($new_obj, $new_obj_id, \%arg_set);
        }

        # Apply init_arg and default for attributes still undefined...
        INIT:
        for my $attr_ref ( @{$attribute{$base_class}} ) {
            # Get arg from initializer list...
            next INIT if defined $attr_ref->{ref}{$new_obj_id};
            if (defined $attr_ref->{init_arg}) {
                $attr_ref->{ref}{$new_obj_id} = $arg_set{$attr_ref->{init_arg}}
                    and next INIT;

            }

            # Or use default value specified...
            $attr_ref->{ref}{$new_obj_id} = $attr_ref->{default};
            next INIT if defined $attr_ref->{ref}{$new_obj_id};

            if (defined $attr_ref->{init_arg}) {
                # Record missing init_arg...
                push @missing_inits, 
                     "Missing initializer label for $base_class: "
                     . "'$attr_ref->{init_arg}'.\n";
                push @suss_keys, keys %arg_set;
            }
        }
    }

    croak @missing_inits,
          _mislabelled(@suss_keys),
          'Fatal error in constructor call'
                if @missing_inits;

    return $new_obj;
}

sub uniq (@) {
    my %seen;
    return grep { $seen{$_}++ } @_;
}


sub _mislabelled {
    my (@names) = map { qq{'$_'} } uniq @_;

    return q{} if @names == 0;

    my $arglist
        = @names == 1 ? $names[0]
        : @names == 2 ? join q{ or }, @names
        :               join(q{, }, @names[0..$#names-1]) . ", or $names[-1]"
        ;
    return "(Did you mislabel one of the args you passed: $arglist?)\n";
}

sub DESTROY {
    my ($self) = @_;
    my $id = ID($self);
    push @_, $id;

    DEMOLISH: for my $base_class (_hierarchy_of(ref $_[0])) {
        if (my $demolish_ref = *{$base_class.'::DEMOLISH'}{CODE}) {
            &{$demolish_ref};
        }

        for my $attr_ref ( @{$attribute{$base_class}} ) {
            delete $attr_ref->{ref}{$id};
        }
    }
}

sub AUTOLOAD {
    my ($invocant) = @_;
    my $invocant_class = ref $invocant || $invocant;
    my ($package_name, $method_name) = $AUTOLOAD =~ m/ (.*) :: (.*) /xms;

    my $ident = ID($invocant);
    if (!defined $ident) { $ident = $invocant }

    for my $parent_class ( _hierarchy_of($invocant_class) ) {
        if (my $automethod_ref = *{$parent_class.'::AUTOMETHOD'}{CODE}) {
            local $CALLER::_ = $_;
            local $_ = $method_name;
            if (my $method_impl
                    = $automethod_ref->($invocant, $ident, @_[1..$#_])) {
                goto &$method_impl;
            }
        }
    }

    my $type = ref $invocant ? 'object' : 'class';
    croak qq{Can't locate $type method "$method_name" via package "$package_name"};
}

{
    my $real_can = \&UNIVERSAL::can;
    no warnings 'redefine';
    *UNIVERSAL::can = sub {
        my ($invocant, $method_name) = @_;

        if (my $method_impl = $real_can->(@_)) {
            return $method_impl;
        }

        for my $parent_class ( _hierarchy_of(ref $invocant || $invocant) ) {
            if (my $automethod_ref = *{$parent_class.'::AUTOMETHOD'}{CODE}) {
                local $CALLER::_ = $_;
                local $_ = $method_name;
                if (my $method_impl = $automethod_ref->(@_)) {
                    return sub { my $inv = shift; $inv->$method_name(@_) }
                }
            }
        }

        return;
    };
}

package Class::Std::SCR;
use base qw( Class::Std );

*ID = \&Scalar::Util::refaddr;

my %values_of  : ATTR( :init_arg<values> );
my %classes_of : ATTR( :init_arg<classes> );

sub new {
    my ($class, $opt_ref) = @_;
    my $new_obj = bless \do{my $scalar}, $class;
    my $new_obj_id = ID($new_obj);
    $values_of{$new_obj_id}  = $opt_ref->{values};
    $classes_of{$new_obj_id} = $opt_ref->{classes};
    return $new_obj;
}

use overload (
    q{""}  => sub { return join q{}, @{$values_of{ID($_[0])}}; },
    q{0+}  => sub { return scalar @{$values_of{ID($_[0])}};    },
    q{@{}} => sub { return $values_of{ID($_[0])};              },
    q{%{}} => sub {
        my ($self) = @_;
        my %hash;
        @hash{@{$classes_of{ID($self)}}} = @{$values_of{ID($self)}};
        return \%hash;
    },
    fallback => 1,
);

1; # Magic true value required at end of module
__END__

=head1 NAME

Class::Std - Support for creating standard "inside-out" classes


=head1 VERSION

This document describes Class::Std version 0.0.2


=head1 SYNOPSIS

    package MyClass;
    use Class::Std;

    # Create storage for object attributes...
    my %name : ATTR;
    my %rank : ATTR;
    my %snum : ATTR;

    my %public_data : ATTR;

    # Handle initialization of objects of this class...
    sub BUILD {
        my ($self, $obj_ID, $arg_ref) = @_;

        $name{$obj_ID} = check_name( $arg_ref->{name} );
        $rank{$obj_ID} = check_rank( $arg_ref->{rank} );
        $snum{$obj_ID} = check_name( _gen_uniq_serial_num() );
    }

    # Handle cleanup of objects of this class...
    sub DEMOLISH {
        my ($self, $obj_ID) = @_;

        _recycle_serial_num( $snum{$obj_ID} );
    }

    # Handle unknown method calls...
    sub AUTOMETHOD {
        my ($self, $obj_ID, @other_args) = @_;

        # Return any public data...
        if ( m/\A get_(.*)/ ) {  # Method name passed in $_
            my $get_what = $1;
            return sub {
                return $public_data{$ident}{$get_what};
            }
        }

        warn "Can't call $method_name on ", ref $self, " object";

        return;   # The call is declined by not returning a sub ref
    }
  
  
=head1 DESCRIPTION

This module provides tools that help to implement the "inside out object"
class structure.

I<Portions of the following code and documentation from "Perl Best Practices"
copyright (c) 2005 by O'Reilly Media, Inc. and reprinted with permission.>

=head2 Introduction

Most programmers who use Perl's object-oriented features construct their
objects by blessing a hash. But, in doing so, they undermine the
robustness of the OO approach. Hash-based objects are unencapsulated:
their entries are open for the world to access and modify.

Objects without effective encapsulation are vulnerable. Instead of
politely respecting their public interface, some clever client coder
inevitably will realize that it's marginally faster to interact directly
with the underlying implementation, pulling out attribute values
directly from the hash of an object:

    for my $file ( get_file_objs() ) {
        print $file->{name}, "\n";
    }

instead of using the official interface:

    for my $file ( get_file_objs() ) {
        print $file->get_name(), "\n";
    }

From the moment someone does that, your class is no longer cleanly
decoupled from the code that uses it. You can't be sure that any bugs in
your class are actually caused by the internals of your class, and not
the result of some kind of monkeying by the client code. And to make
matters worse, now you can't ever change those internals without the
risk of breaking some other part of the system.

There is a simple, convenient, and utterly secure way to prevent client
code from accessing the internals of the objects you provide. Happily,
that approach also guards against misspelling attribute names (a common
error in hash-based classes), as well as being just as fast as--and
often more memory-efficient than--ordinary hash-based objects.

That approach is referred to by various names--flyweight scalars,
warehoused attributes, inverted indices--but most commonly it's known
as: inside-out objects. Consider the following class definitions:

    package File::Hierarchy;
    {
        # Objects of this class have the following attributes...
        my %root_of;   # The root directory of the file hierarchy
        my %files_of;  # Array storing object for each file in root directory
        
        # Constructor takes path of file system root directory...
        sub new {
            my ($class, $root) = @_;
        
            # Bless a scalar to instantiate the new object...
            my $new_object = bless \do{my $anon_scalar}, $class;
        
            # Initialize the object's "root" attribute...
            $root_of{ident $new_object} = $root;
        
            return $new_object;
        }
        
        # Retrieve files from root directory...
        sub get_files {
            my ($self) = @_;
        
            # Load up the "files" attribute, if necessary...
            if (!exists $files_of{ident $self}) {
                $files_of{ident $self} 
                    = File::System->list_files($root_of{ident $self});
            }
        
            # Flatten the "files" attribute's array to produce a file list...
            return @{ $files_of{ident $self} };
        }
    }

    package File::Hierarchy::File;
    {    
        # Objects of this class have the following attributes...
        my %name_of;  # the name of the file
        
        # Constructor takes name of file...
        sub new {
            my ($class, $filename) = @_;
        
            # Bless a scalar to instantiate the new object...
            my $new_object = bless \do{my $anon_scalar}, $class;
        
            # Initialize the object's "name" attribute...
            $name_of{ident $new_object} = $filename;
        
            return $new_object;
        }
        
        # Retrieve name of file...
        sub get_name {
            my ($self) = @_;
        
            return $name_of{ident $self};
        }
    }

Unlike a hash-based class, each of these inside-out class is specified
inside a surrounding code block:

    package File::Hierarchy;
    {
        # [Class specification here]
    }

    package File::Hierarchy::File;
    {
        # [Class specification here]
    }

That block is vital, because it creates a limited scope, to which any
lexical variables that are declared as part of the class will
automatically be restricted. 

The next difference between the two versions of the classes is that each 
attribute of I<all> the objects in the class is now stored in a separate
single hash:

    # Objects of this class have the following attributes...

    my %root_of;   # The root directory of the file hierarchy
    my %files_of;  # Array storing object for each file in root directory

This is 90 degrees to the usual hash-based approach. In hash-based
classes, all the attributes of one object are stored in a single hash;
in inside-out classes, one attribute from all objects is stored in a
single hash. Diagrammatically:

    Hash-based:
                     Attribute 1      Attribute 2

     Object A    { attr1 => $valA1,  attr2 => $val2 }

     Object B    { attr1 => $valB1,  attr2 => $val2 }

     Object C    { attr1 => $valB1,  attr2 => $val2 }



    Inside-out:
                      Object A           Object B          Object C

    Attribute 1  { 19817 => $valA1,  172616 => $valB1,  67142 => $valC1 }

    Attribute 2  { 19817 => $valA2,  172616 => $valB2,  67142 => $valC3 }

    Attribute 3  { 19817 => $valA3,  172616 => $valB3,  67142 => $valC3 }

So the attributes belonging to each object are distributed across a set of
predeclared hashes, rather than being squashed together into one anonymous
hash.

This is a significant improvement. By telling Perl what attributes you
expect to use, you enable the compiler to check--via use strict--that
you do indeed use only those attributes.

That's because of the third difference in the two approaches. Each
attribute of a hash-based object is stored in an entry in the object's
hash: C<< $self->{name} >>. In other words, the name of a hash-based attribute
is symbolic: specified by the string value of a hash key. In contrast,
each attribute of an inside-out object is stored in an entry of the
attribute's hash: C<$name_of{ident $self}>. So the name of an inside-out
attribute isn't symbolic; it's a hard-coded variable name.

With hash-based objects, if an attribute name is accidentally misspelled
in some method:

    sub set_name {
        my ($self, $new_name) = @_;

        $self->{naem} = $new_name;             # Oops!

        return;
    }

then the C<$self> hash will obligingly--and silently!--create a new entry
in the hash, with the key C<'naem'>, then assign the new name to it. But
since every other method in the class correctly refers to the attribute
as C<$self->{name}>, assigning the new value to C<$self->{naem}> effectively
makes that assigned value "vanish".

With inside-out objects, however, an object's "name" attribute is stored
as an entry in the class's lexical C<%name_of> hash. If the attribute name
is misspelled then you're attempting to refer to an entirely different
hash: C<%naem_of>. Like so:

    sub set_name {
        my ($self, $new_name) = @_;

        $naem_of{ident $self} = $new_name;     # Kaboom!

        return;
    }

But, since there's no such hash declared in the scope, use strict will
complain (with extreme prejudice):

    Global symbol "%naem_of" requires explicit package name at Hierarchy.pm line 86

Not only is that consistency check now automatic, it's also performed at
compile time.

The next difference is even more important and beneficial. Instead of
blessing an empty anonymous hash as the new object:

    my $new_object = bless {}, $class;

the inside-out constructor blesses an empty anonymous scalar:

    my $new_object = bless \do{my $anon_scalar}, $class;

That odd-looking C<\do{my $anon_scalar}> construct is needed because
there's no built-in syntax in Perl for creating a reference to an
anonymous scalar; you have to roll-your-own.
    
The anonymous scalar is immediately passed to bless, which anoints it as
an object of the appropriate class. The resulting object reference is
then stored in C<$new_object>.

Once the object exists, it's used to create a unique key
(C<ident $new_object>) under which each attribute that belongs to the
object will be stored (e.g. C<$root_of{ident $new_object}> or
C<$name_of{ident $self}>). The C<ident()> utility that produces this unique
key is provided by the Class::Std module and is identical in effect to
the C<refaddr()> function in the standard Scalar::Util module.

To recap: every inside-out object is a blessed scalar, and
has--intrinsic to it--a unique identifying integer. That integer can be
obtained from the object reference itself, and then used to access a
unique entry for the object in each of the class's attribute hashes.

This means that every inside-out object is nothing more than an
unintialized scalar. When your constructor passes a new inside-out
object back to the client code, all that comes back is an empty scalar,
which makes it impossible for that client code to gain direct access to
the object's internal state.

Of the several popular methods of reliably enforcing encapsulation in
Perl, inside-out objects are also by far the cheapest. The run-time
performance of inside-out classes is effectively identical to that of
regular hash-based classes. In particular, in both schemes, every
attribute access requires only a single hash look-up. The only
appreciable difference in speed occurs when an inside-out object is
destroyed.

Hash-based classes usually don't even have destructors. When the
object's reference count decrements to zero, the hash is automatically
reclaimed, and any data structures stored inside the hash are likewise
cleaned up. This works so well that many OO Perl programmers find they
never need to write a C<DESTROY()> method; Perl's built-in garbage
collection handles everything just fine. In fact, the only time a
destructor is needed is when objects have to manage resources outside
that are not actually located inside the object, resources that need to
be separately deallocated.

But the whole point of an inside-out object is that its attributes are
stored in allocated hashes that are not actually located inside the
object. That's precisely how it achieves secure encapsulation: by not
sending the attributes out into the client code.

Unfortunately, that means when an inside-out object is eventually
garbage collected, the only storage that is reclaimed is the single
blessed scalar implementing the object. The object's attributes are
entirely unaffected by the object's deallocation, because the attributes
are not inside the object, nor are they referred to by it in any way.

Instead, the attributes are referred to by the various attribute hashes
in which they're stored. And since those hashes will continue to exist
until the end of the program, the defunct object's orphaned attributes
will likewise continue to exist, safely nestled inside their respective
hashes, but now untended by any object. In other words, when an inside-
out object dies, its associated attribute hashes leak memory.

The solution is simple. Every inside-out class has to provide a
destructor that "manually" cleans up the attributes of the object being
destructed:

    package File::Hierarchy;
    {
        # Objects of this class have the following attributes...
        my %root_of;   # The root directory of the file hierarchy
        my %files_of;  # Array storing object for each file in root directory
        
        # Constructor takes path of file system root directory...
        sub new {
            # As before
        }
        
        # Retrieve files from root directory...
        sub get_files {
            # As before
        }

        # Clean up attributes when object is destroyed...
        sub DESTROY {
            my ($self) = @_;

            delete $root_of{ident $self};
            delete $files_of{ident $self};
        }
    }

The obligation to provide a destructor like this in every inside-out
class can be mildly irritating, but it is still a very small price to
pay for the considerable benefits that the inside-out approach otherwise
provides for free. And the irritation can easily be eliminated by using
the appropriate class construction tools. See below.

=head2 Automating Inside-Out Classes

Perhaps the most annoying part about building classes in Perl (no matter how
the objects are implemented) is that the basic structure of every class is 
more or less identical. For example, the implementation of the
C<File::Hierarchy::File> class used in C<File::Hierarchy> looks like this:

    package File::Hierarchy::File;
    {    
        # Objects of this class have the following attributes...
        my %name_of;  # the name of the file
        
        # Constructor takes name of file...
        sub new {
            my ($class, $filename) = @_;
        
            # Bless a scalar to instantiate the new object...
            my $new_object = bless \do{my $anon_scalar}, $class;
        
            # Initialize the object's "name" attribute...
            $name_of{ident $new_object} = $filename;
        
            return $new_object;
        }
        
        # Retrieve name of file...
        sub get_name {
            my ($self) = @_;
        
            return $name_of{ident $self};
        }

        # Clean up attributes when object is destroyed...
        sub DESTROY {
            my ($self) = @_;

            delete $name_of{ident $self};
        }
    }

Apart from the actual names of the attributes, and their accessor methods,
that's exactly the same structure, and even the same code, as in the
C<File::Hierarchy> class.

Indeed, the standard infrastructure of I<every> inside-out class looks
exactly the same. So it makes sense not to have to rewrite that standard
infrastructure code in every separate class.

That's precisely what is module does: it implements the necessary
infrastructure for inside-out objects. See below.


=head1 INTERFACE 

=head2 Exported subroutines

=over 

=item C<ident()>

Class::Std always exports a subroutine called C<ident()>. This subroutine
returns a unique integer ID for any object passed to it. 

=back


=head2 Methods created automatically

The following subroutines are installed in any class that uses the
Class::Std module.

=over

=item C<new()>

Every class that loads the Class::Std module automatically has a C<new()>
constructor, which returns an inside-out object (i.e. a blessed scalar).

    $obj = MyClass->new();

The constructor can be passed a single argument to initialize the
object. This argument must be a hash reference. 

    $obj = MyClass->new({ name=>'Foo', location=>'bar' });

See the subsequent descriptions of the C<BUILD()> method and C<:ATTR()>
trait, for an explanation of how the contents of this optional hash can
be used to initialize the object.

It is almost always an error to implement your own C<new()> in any class
that uses Class::Std. You almost certainly want to write a C<BUILD()> method
instead. See below.


=item C<DESTROY()>

Every class that loads the Class::Std module automatically has a C<DESTROY()>
destructor, which automatically cleans up any attributes declared with the
C<:ATTR()> trait (see below).

It is almost always an error to write your own C<DESTROY()> in any class that
uses Class::Std. You almost certainly want to write your own C<DEMOLISH()>
instead. See below.


=item C<AUTOLOAD()>

Every class that loads the Class::Std module automatically has an
C<AUTOLOAD()> method, which implements the C<AUTOMETHOD()> mechanism
described below. 

It is almost always an error to write your own C<AUTOLOAD()> in any class that
uses Class::Std. You almost certainly want to write your own C<AUTOMETHOD()>
instead.

=item C<_DUMP()>

This method returns a string that represents the internal state (i.e. the
attribute values) of the object on which it's called. Only those attributes
which are marked with an C<:ATTR> (see below) are reported. Attribute names
are reported only if they can be ascertained from an C<:init_arg>, C<:get>, or
C<:set> option within the C<:ATTR()>.

=back


=head2 Methods that can be supplied by the developer

The following subroutines can be specified as standard methods of a
Class::Std class.

=over

=item C<BUILD()>

When the C<new()> constructor of a Class::Std class is called, it
automatically calls every method named C<BUILD()> in I<all> the classes
in the new object's hierarchy. That is, when the constructor is called,
it walks the class's inheritance tree (from base classes downwards) and
calls every C<BUILD()> method it finds along the way.

This means that, to initialize any class, you merely need to provide a
C<BUILD()> method for that class. You don't have to worry about ensuring
that any ancestral C<BUILD()> methods also get called; the constructor
will take care of that.

Each C<BUILD()> method is called with three arguments: the invocant object,
the identifier number of that object, and a reference to (a customized version
of) the hash of arguments that was originally passed to the constructor:

    sub BUILD {
        my ($self, $ident, $args_ref) = @_;
        ...
    }

The argument hash is a "customized version" because the module
automatically does some fancy footwork to ensure that the arguments are
the ones appropriate to the class itself. That's because there's a
potential for collisions when Class::Std classes are used in a
hierarchy.

One of the great advantages of using inside-out classes instead of hash-based
classes is that an inside-out base class and an inside-out derived
class can then each have an attribute of exactly the same name, which
are stored in separate lexical hashes in separate scopes. In a hash-based
object that's impossible, because the single hash can't have two
attributes with the same key.

But that very advantage also presents something of a problem when
constructor arguments are themselves passed by hash. If two or more
classes in the name hierarchy do happen to have attributes of the same
name, the constructor will need two or more initializers with the name
key. Which a single hash can't provide.

The solution is to allow initializer values to be partitioned into
distinct sets, each uniquely named, and which are then passed to the
appropriate base class. The easiest way to accomplish that is to pass
in a hash of hashes, where each top level key is the name of one of
the base classes, and the corresponding value is a hash of
initializers specifically for that base class. 

For example:

    package Client;
    use Class::Std::Utils;
    {
        my %client_num_of :ATTR;  # Every client has a basic ID number
        my %name_of       :ATTR;

        sub BUILD {
            my ($self, $ident, $arg_ref) = @_;

            $client_num_of{$ident} = $arg_ref->{'Client'}{client_num};
            $name_of{$ident}       = $arg_ref->{'Client'}{client_name};
        }
    }

    package Client::Corporate;
    use base qw( Client );
    use Class::Std::Utils;
    {
        my %client_num_of;     # Corporate clients have an additional ID number
        my %corporation_of;
        my %position_of; 

        sub BUILD {
            my ($self, $ident, $arg_ref) = @_;

            $client_num_of{$ident} 
                = $arg_ref->{'Client::Corporate'}{client_num};
            $corporation_of{$ident}
                = $arg_ref->{'Client::Corporate'}{corp_name};
            $position_of{$ident}
                = $arg_ref->{'Client::Corporate'}{position};
        }
    }

    # and later...

    my $new_client 
        = Client::Corporate->new( {
            'Client' => { 
                client_num  => '124C1', 
                client_name => 'Humperdinck',
            },
            'Client::Corporate' => { 
                client_num  => 'F_1692', 
                corp_name   => 'Florin', 
                position    => 'CEO',
            },
        });

Now each class's C<BUILD()> method picks out only the initializer sub-hash
whose key is that class's own name. Since every class name is
different, the top-level keys of this multi-level initializer hash are
guaranteed to be unique. And since no single class can have two
identically named attributes, the keys of each second-level hash will be
unique as well. If two classes in the hierarchy both need an initializer
of the same name (e.g. 'client_num'), those two hash entries will now be
in separate sub-hashes, so they will never clash.

Class::Std provides an even more sophisticated variation on this
functionality, which is generally much more convenient for the users of
classes. Classes that use Class::Std infrastructure allow both general
and class-specific initializers in the initialization hash. Clients only
need to specify classes for those initializers whose names actually are
ambiguous. Any other arguments can just be passed directly in the
top-level hash:

    my $new_client 
        = Client::Corporate->new( {
            client_name => 'Humperdinck',
            corp_name   => 'Florin', 
            position    => 'CEO',

            'Client'            => { client_num  => '124C1'  }, 
            'Client::Corporate' => { client_num  => 'F_1692' },
        });

Class::Std also makes it easy for each class's C<BUILD()> to access
these class-specific initializer values. Before each C<BUILD()> is
invoked, the nested hash whose key is the same as the class name is
flattened back into the initializer hash itself. That is, C<Client::BUILD()>
is passed the hash:

    {
        client_name => 'Humperdinck',
        corp_name   => 'Florin', 
        position    => 'CEO',
        client_num  => '124C1',   # Flattened from 'Client' nested subhash

        'Client'            => { client_num  => '124C1'  }, 
        'Client::Corporate' => { client_num  => 'F_1692' },
    }

whereas C<Client::Corporate::BUILD()> is passed the hash:

    {
        client_name => 'Humperdinck',
        corp_name   => 'Florin', 
        position    => 'CEO',
        client_num  => 'F_1692',   # Flattened from 'Client::Corporate' subhash

        'Client'            => { client_num  => '124C1'  }, 
        'Client::Corporate' => { client_num  => 'F_1692' },
    }

This means that the C<BUILD()> method for each class can just assume that the
correct class-specific initializer values will available at the top level of
the hash. For example:

        sub Client::BUILD {
            my ($self, $ident, $arg_ref) = @_;

            $client_num_of{$ident} = $arg_ref->{client_num};    # '124C1'
            $name_of{$ident}       = $arg_ref->{client_name};
        }

        sub Client::Corporate::BUILD {
            my ($self, $ident, $arg_ref) = @_;

            $client_num_of{$ident}  = $arg_ref->{client_num};   # 'F_1692'
            $corporation_of{$ident} = $arg_ref->{corp_name};
            $position_of{$ident}    = $arg_ref->{position};
        }

Both classes use the C<< $arg_ref->{client_num} >> initializer value, but
Class::Std automatically arranges for that value to be the right one for each
class.

Also see the C<:ATTR()> marker (described below) for a simpler way of
initializing attributes.


=item C<DEMOLISH()>

The C<DESTROY()> method that is automatically provided by Class::Std ensures
that all the marked attributes (see the C<:ATTR()> marker below) of an object,
from all the classes in its inheritance hierarchy, are automatically cleaned
up.

But, if a class requires other destructor behaviours (e.g. closing
filehandles, decrementing allocation counts, etc.) then you may need to
specify those explicitly.

Whenever an object of a Class::Std class is destroyed, the C<DESTROY()>
method supplied by Class::Std automatically calls every method named
C<DEMOLISH()> in I<all> the classes in the new object's hierarchy. That
is, when the destructor is called, it walks the class's inheritance
tree (from derived classes upwards) and calls every C<DEMOLISH()> method it
finds along the way.

This means that, to clean up any class, you merely need to provide a
C<DEMOLISH()> method for that class. You don't have to worry about ensuring
that any ancestral C<DEMOLISH()> methods also get called; the destructor
will take care of that.

Each C<DEMOLISH()> method is called with two arguments: the invocant object,
and the identifier number of that object. For example:

    sub DEMOLISH {
        my ($self, $ident) = @_;

        $filehandle_of{$ident}->flush();
        $filehandle_of{$ident}->close();
    }

Note that the attributes of the object are cleaned up I<after> the
C<DEMOLISH()> method is complete, so they may still be used within
that method.


=item C<AUTOMETHOD()>

There is a significant problem with Perl's built-in C<AUTOLOAD> mechanism:
there's no way for a particular C<AUTOLOAD()> to say "no".

If two or more classes in a class hierarchy have separate C<AUTOLOAD()>
methods, then the one belonging to the left-most-depth-first class in
the inheritance tree will always be invoked in preference to any others.
If it can't handle a particular call, the call will probably fail
catastrophically. This means that derived classes can't always be used
in place of base classes (a feature known as "Liskov substitutability")
because their inherited autoloading behaviour may be pre-empted by some
other unrelated base class on their left in the hierarchy.

Class::Std provides a mechanism that solves this problem: the
C<AUTOMETHOD> method. An AUTOMETHOD() is expected to return either a
handler subroutine that implements the requested method functionality,
or else an C<undef> to indicate that it doesn't know how to handle the
request. Class::Std then coordinates every C<AUTOMETHOD()> in an object's
hierarchy, trying each one in turn until one of them produces a
suitable handler.

The advantage of this approach is that the first C<AUTOMETHOD()> that's
invoked doesn't have to disenfranchise every other C<AUTOMETHOD()> in the
hierarchy. If the first one can't handle a particular method call, it
simply declines it and Class::Std tries the next candidate instead.

Using C<AUTOMETHOD()> instead of C<AUTOLOAD()> makes a class
cleaner, more robust, and less disruptive in class hierarchies.
For example:

    package Phonebook;
    use Class::Std;
    {
        my %entries_of : ATTR;

        # Any method call is someone's name:
        # so store their phone number or get it...
        sub AUTOMETHOD {
            my ($self, $ident, $number) = @_;

            my $subname = $_;   # Requested subroutine name is passed via $_

            # Return failure if not a get_<name> or set_<name>
            # (Next AUTOMETHOD() in hierarchy will then be tried instead)...
            my ($mode, $name) = $subname =~ m/\A ([gs]et)_(.*) \z/xms
                or return;

            # If get_<name>, return a handler that just returns the old number...
            return sub { return $entries_of{$ident}->{$name}; }
                if $mode eq 'get';

            # Otherwise, set_<name>, so return a handler that
            # updates the entry and then returns the old number...
            return sub {
                $entries_of{$ident}->{$name} = $number;
                return;
            };
        }
    }

    # and later...

    my $lbb = Phonebook->new();

    $lbb->set_Jenny(867_5309);
    $lbb->set_Glenn(736_5000);

    print $lbb->get_Jenny(), "\n";
    print $lbb->get_Glenn(), "\n";

Note that, unlike C<AUTOLOAD()>, an C<AUTOMETHOD()> is called with both the
invocant and the invocant's unique C<ident> number, followed by the actual
arguments that were passed to the method.

Note too that the name of the method being called is passed as C<$_>
instead of C<$AUTOLOAD>, and does I<not> have the class name prepended
to it, so you don't have to strip that name off the front like almost
everyone almost always does in their C<AUTOLOAD()>. If your C<AUTOMETHOD()>
also needs to access the C<$_> from the caller's scope, that's still
available as C<$CALLER::_>.

=back


=head2 Variable traits that can be ascribed

The following markers can be added to the definition of any hash
used as an attribute storage within a Class::Std class

=over

=item C<:ATTR()>

This marker can be used to indicate that a lexical hash is being used
to store one particular attribute of all the objects of the class. That is:

    package File::Hierarchy;
    {
        my %root_of  :ATTR;  
        my %files_of :ATTR;
        
        # etc.
    }

    package File::Hierarchy::File;
    {    
        my %name_of;  :ATTR;

        # etc.
    }

Adding the C<:ATTR> marker to an attribute hash ensures that the corresponding 
attribute belonging to each object of the class is automatically cleaned up 
when the object is destroyed.

The C<:ATTR> marker can also be given a number of options which automate
other attribute-related behaviours. Each of these options consists of a
key/value pair, which may be specified in either Perl 5 "fat comma" syntax
( C<< S<< key => 'value' >> >> ) or in one of the Perl 6 option syntaxes
( C<< S<< :key<value> >> >> or C<< S<< :key('value') >> >> or 
C<< S<< :key«value» >> >>).

=over

=item C<< :ATTR( :init_arg<initializer_key> ) >>

This option tells Class::Std which key in the constructor's initializer hash
holds the value with which the marked attribute should be initialized. That
is, instead of writing:

    my %rank_of :ATTR;

    sub BUILD {
        my ($self, $ident, $arg_ref) = @_;

        $rank_of{$ident} = $arg_ref->{rank};
    }

you can achieve the same initialization, by having Class::Std I<automatically>
pull that entry out of the hash and store it in the right attribute:

    my %rank_of :ATTR( :init_arg<rank> );

    # No BUILD() method required


=item C<< :ATTR( :default<compile_time_default_value> ) >>

If a marked attribute is not initialized (either directly within a
C<BUILD()>, or automatically via an C<:init_arg> option), the constructor
supplied by Class::Std checks to see if a default value was specified
for that attribute. If so, that value is assigned to the attribute.

So you could replace:

    my %seen_of :ATTR;

    sub BUILD {
        my ($self, $ident, $arg_ref) = @_;

        $seen_of{$ident} = 0;  # Not seen yet
    }

with:

    my %seen_of :ATTR( :default(0) );

    # No BUILD() required


=item C<< :ATTR( :get<name> ) >>

If the C<:get> option is specified, a read accessor is created for the
corresponding attribute. The name of the accessor is C<get_> followed by
whatever name is specified as the value of the C<:get> option. For example,
instead of:

    my %current_count_of :ATTR;

    sub get_count {
        my ($self) = @_;

        return $current_count_of{ident($self)};
    }

you can just write:

    my %count_of :ATTR( :get<count> );

Note that there is no way to prevent Class::Std adding the initial C<get_> to
each accessor name it creates. That's what "standard" means. See Chapter 15
of I<Perl Best Practices> (O'Reilly, 2005) for a full discussion on why
accessors should be named this way.

=item C<< :ATTR( :set<name> ) >>

If the C<:set> option is specified, a write accessor is created for the
corresponding attribute. The name of the accessor is C<set_> followed by
whatever name is specified as the value of the C<:set> option. For example,
instead of:

    my %current_count_of :ATTR;

    sub set_count {
        my ($self, $new_value) = @_;

        croak "Missing new value in call to 'set_count' method"
            unless @_ == 2;

        $current_count_of{ident($self)} = $new_value;
    }

you can just write:

    my %count_of :ATTR( :set<count> );

Note that there is no way to prevent Class::Std adding the initial
C<set_> to each accessor name it creates. Nor is there any way to create
a combined "getter/setter" accessor. See Chapter 15 of I<Perl Best
Practices> (O'Reilly, 2005) for a full discussion on why accessors
should be named and implemented this way.

=back

You can, of course, specify two or more arguments in a single C<:ATTR()>
specification:

    my %rank_of : ATTR( :init_arg<rank>  :get<rank>  :set<rank> );


=item C<:ATTRS()>

This is just another name for the C<:ATTRS> marker (see above). The plural
form is convenient when you want to specify a series of attribute hashes in
the same statement:

    my (
        %name_of,
        %rank_of,
        %snum_of,
        %age_of,
        %unit_of,
        %assignment_of,
        %medals_of,
    ) : ATTRS;

=back

=head2 Method traits that can be ascribed

The following markers can be added to the definition of any subroutine
used as a method within a Class::Std class

=over

=item C<:CUMULATIVE()>

One of the most important advantages of using the C<BUILD()> and C<DEMOLISH()>
mechanisms supplied by Class::Std is that those methods don't require
nested calls to their ancestral methods, via the C<SUPER> pseudo-class. The
constructor and destructor provided by Class::Std take care of the
necessary redispatching automatically. Each C<BUILD()> method can focus
solely on its own responsibilities; it doesn't have to also help
orchestrate the cumulative constructor effects across the class
hierarchy by remembering to call C<< $self->SUPER::BUILD() >>.

Moreover, calls via C<SUPER> can only ever call the method of exactly one
ancestral class, which is not sufficient under multiple inheritance.

Class::Std provides a different way of creating methods whose effects
accumulate through a class hierarchy, in the same way as those of
C<BUILD()> and C<DEMOLISH()> do. Specifically, the module allows you to define
your own "cumulative methods".

An ordinary non-cumulative method hides any method of the same name
inherited from any base class, so when a non-cumulative method is
called, only the most-derived version of it is ever invoked. In
contrast, a cumulative method doesn't hide ancestral methods of the same
name; it assimilates them. When a cumulative method is called, the
most-derived version of it is invoked, then any parental versions, then any
grandparental versions, etc. etc, until every cumulative method of the
same name throughout the entire hierarchy has been called.

For example, you could define a cumulative C<describe()> method to the various
classes in a simple class hierarchy like so:

    package Wax::Floor;
    use Class::Std;
    {
        my %name_of    :ATTR( init_arg => 'name'   );
        my %patent_of  :ATTR( init_arg => 'patent' );

        sub describe :CUMULATIVE {
            my ($self) = @_;

            print "The floor wax $name_of{ident $self} ",
                  "(patent: $patent_of{ident $self})\n";

            return;
        }
    }

    package Topping::Dessert;
    use Class::Std;
    {
        my %name_of     :ATTR( init_arg => 'name'    );
        my %flavour_of  :ATTR( init_arg => 'flavour' );

        sub describe :CUMULATIVE {
            my ($self) = @_;

            print "The dessert topping $name_of{ident $self} ",
                  "with that great $flavour_of{ident $self} taste!\n";

            return;
        }
    }

    package Shimmer;
    use base qw( Wax::Floor  Topping::Dessert );
    use Class::Std;
    {
        my %name_of    :ATTR( init_arg => 'name'   );
        my %patent_of  :ATTR( init_arg => 'patent' );

        sub describe :CUMULATIVE {
            my ($self) = @_;

            print "New $name_of{ident $self} ",
                  "(patent: $patent_of{ident $self})\n",
                  "Combining...\n";

            return;
        }
    }

Because the various C<describe()> methods are marked as being cumulative, a
subsequent call to:

    my $product 
        = Shimmer->new({
              name    => 'Shimmer',
              patent  => 1562516251,
              flavour => 'Vanilla',
          });

    $product->describe();

will work its way up through the classes of Shimmer's inheritance tree
(in the same order as a destructor call would), calling each C<describe()>
method it finds along the way. So the single call to C<describe()> would
invoke the corresponding method in each class, producing:

    New Shimmer (patent: 1562516251)
    Combining...
    The floor wax Shimmer (patent: 1562516251)
    The dessert topping Shimmer with that great Vanilla taste!

Note that the accumulation of C<describe()> methods is hierarchical, and
dynamic in nature. That is, each class only sees those cumulative
methods that are defined in its own package or in one of its ancestors.
So calling the same C<describe()> on a base class object:

    my $wax 
        = Wax::Floor->new({ name=>'Shimmer ', patent=>1562516251 });

    $wax->describe();

only invokes the corresponding cumulative methods from that point on up
the hierarchy, and hence only prints:

    The floor wax Shimmer (patent: 1562516251)

Cumulative methods also accumulate their return values. In a list
context, they return a (flattened) list that accumulates the lists
returned by each individual method invoked.

In a scalar context, a set of cumulative methods returns an object that,
in a string context, concatenates individual scalar returns to produce a
single string. When used as an array reference that same scalar-context-return
object acts like an array of the list context values. When used as a hash
reference, the object acts like a hash whose keys are the classnames from the
object's hierarchy, and whose corresponding values are the return values of
the cumulative method from that class.

For example, if the classes each have a cumulative method that returns
their list of sales features:

    package Wax::Floor;
    use Class::Std;
    {
        sub feature_list :CUMULATIVE {
            return ('Long-lasting', 'Non-toxic', 'Polymer-based');
        }
    }

    package Topping::Dessert;
    use Class::Std;
    {
        sub feature_list :CUMULATIVE {
            return ('Low-carb', 'Non-dairy', 'Sugar-free');
        }
    }

    package Shimmer;
    use Class::Std;
    use base qw( Wax::Floor  Topping::Dessert );
    {
        sub feature_list :CUMULATIVE {
            return ('Multi-purpose', 'Time-saving', 'Easy-to-use');
        }
    }

then calling feature_list() in a list context:

    my @features = Shimmer->feature_list();
    print "Shimmer is the @features alternative!\n";

would produce a concatenated list of features, which could then be
interpolated into a suitable sales-pitch:

    Shimmer is the Multi-purpose Time-saving Easy-to-use
    Long-lasting Non-toxic Polymer-based Low-carb Non-dairy
    Sugar-free alternative!

It's also possible to specify a set of cumulative methods that
start at the base class(es) of the hierarchy and work downwards, the way
BUILD() does. To get that effect, you simply mark each method with
:CUMULATIVE(BASE FIRST), instead of just :CUMULATIVE. For example:

    package Wax::Floor;
    use Class::Std;
    {
        sub active_ingredients :CUMULATIVE(BASE FIRST) {
            return "\tparadichlorobenzene, cyanoacrylate, peanuts\n";
        }
    }

    package Topping::Dessert;
    use Class::Std;
    {
        sub active_ingredients :CUMULATIVE(BASE FIRST) {
            return "\tsodium hypochlorite, isobutyl ketone, ethylene glycol\n";
        }
    }

    package Shimmer;
    use Class::Std;
    use base qw( Wax::Floor  Topping::Dessert );

    {
        sub active_ingredients :CUMULATIVE(BASE FIRST) {
            return "\taromatic hydrocarbons, xylene, methyl mercaptan\n";
        }
    }

So a scalar-context call to active_ingredients():

    my $ingredients = Shimmer->active_ingredients();
    print "May contain trace amounts of:\n$ingredients";

would start in the base classes and work downwards, concatenating base-
class ingredients before those of the derived class, to produce:

    May contain trace amounts of:
        paradichlorobenzene, cyanoacrylate, peanuts
        sodium hypochlorite, isobutyl ketone, ethylene glycol
        aromatic hydrocarbons, xylene, methyl mercaptan

Or, you could treat the return value as a hash:

    print Data::Dumper::Dumper \%{$ingredients};

and see which ingredients came from where:

    $VAR1 = {
       'Shimmer'
            => 'aromatic hydrocarbons, xylene, methyl mercaptan',

       'Topping::Dessert'
            => 'sodium hypochlorite, isobutyl ketone, ethylene glycol',

        'Wax::Floor'
            => 'Wax: paradichlorobenzene,  hydrogen peroxide, cyanoacrylate',
    };

Note that you can't specify both C<:CUMULATIVE> and C<:CUMULATIVE(BASE
FIRST)> on methods of the same name in the same hierarchy. The resulting
set of methods would have no well-defined invocation order, so
Class::Std throws a compile-time exception instead.


=item C<:STRINGIFY>

If you define a method and add the C<:STRINGIFY> marker then that method
is used whenever an object of the corresponding class needs to be
coerced to a string. In other words, instead of:

    # Convert object to a string...
    sub as_str {
        ...
    }

    # Convert object to a string automatically in string contexts...
    use overload (
        q{""}    => 'as_str',
        fallback => 1,
    );

you can just write:

    # Convert object to a string (automatically in string contexts)...
    sub as_str : STRINGIFY {
        ...
    }


=item C<:NUMERIFY>

If you define a method and add the C<:NUMERIFY> marker then that method
is used whenever an object of the corresponding class needs to be
coerced to a number. In other words, instead of:

    # Convert object to a number...
    sub as_num {
        ...
    }

    # Convert object to a string automatically in string contexts...
    use overload (
        q{0+}    => 'as_num',
        fallback => 1,
    );

you can just write:

    # Convert object to a number (automatically in numeric contexts)...
    sub as_num : NUMERIFY {
        ...
    }


=item C<:BOOLIFY>

If you define a method and add the C<:BOOLIFY> marker then that method
is used whenever an object of the corresponding class needs to be
coerced to a boolean value. In other words, instead of:

    # Convert object to a boolean...
    sub as_bool {
        ...
    }

    # Convert object to a boolean automatically in boolean contexts...
    use overload (
        q{bool}    => 'as_bool',
        fallback => 1,
    );

you can just write:

    # Convert object to a boolean (automatically in boolean contexts)...
    sub as_bool : BOOLIFY {
        ...
    }


=item C<:SCALARIFY>

=item C<:ARRAYIFY>

=item C<:HASHIFY>

=item C<:GLOBIFY>

=item C<:CODIFY>

If a method is defined with one of these markers, then it is automatically
called whenever an object of that class is treated as a reference of the
corresponding type.

For example, instead of:

    sub as_hash {
        my ($self) = @_;

        return {
            age      => $age_of{ident $self},
            shoesize => $shoe_of{ident $self},
        };
    }

    use overload (
        '%{}'    => 'as_hash',
        fallback => 1,
    );

you can just write:

    sub as_hash : HASHIFY {
        my ($self) = @_;

        return {
            age      => $age_of{ident $self},
            shoesize => $shoe_of{ident $self},
        };
    }

Likewise for methods that allow an object to be treated as a scalar
reference (C<:SCALARIFY>), a array reference (C<:ARRAYIFY>), a
subroutine reference (C<:CODIFY>), or a typeglob reference
(C<:GLOBIFY>).

=back


=head1 DIAGNOSTICS

=over 

=item Can't find class %s

You tried to call the Class::Std::new() constructor on a class 
that isn't built using Class::Std. Did you forget to write C<use Class::Std>
after the package declaration?

=item Argument to %s->new() must be hash reference

The constructors created by Class::Std require all initializer values
to be passed in a hash, but you passed something that wasn't a hash.
Put your constructor arguments in a hash.

=item Missing initializer label for %s: %s

You specified that one or more attributes had initializer values (using the
C<init> argument inside the attribute's C<ATTR> marker), but then failed
to pass in the corresponding initialization value. Often this happens because
the initialization value I<was> passed, but the key specifying the
attribute name was misspelled.

=item Can't make anonymous subroutine cumulative

You attempted to use the C<:CUMULATIVE> marker on an anonymous subroutine.
But that marker can only be applied to the named methods of a class. Convert
the anonymous subroutine to a named subroutine, or find some other way to 
make it interoperate with other methods.

=item Conflicting definitions for cumulative method: %s

You defined a C<:CUMULATIVE> and a C<:CUMULATIVE(BASE FIRST)> method of the
same name in two classes within the same hierarchy. Since methods can only be
called going strictly up through the hierarchy or going strictly down 
through the hierarchy, specifying both directions is obviously a mistake.
Either rename one of the methods, or decide whether they should accumulate
upwards or downwards.

=item Missing new value in call to 'set_%s' method

You called an attribute setter method without providing a new value 
for the attribute. Often this happens because you passed an array that
happened to be empty. Make sure you pass an actual value.

=item Can't locate %s method "%s" via package %s

You attempted to call a method on an object but no such method is defined
anywhere in the object's class hierarchy. Did you misspell the method name, or
perhaps misunderstand which class the object belongs to?

=item Internal error: %s

Your code is okay, but it uncovered a bug in the Class::Std module.
L<BUGS AND LIMITATIONS> explains how to report the problem.

=back


=head1 CONFIGURATION AND ENVIRONMENT

Class::Std requires no configuration files or environment variables.


=head1 DEPENDENCIES

Class::Std depends on the following modules:

=over

=item *

version

=item *

Scalar::Util

=item *

List::Util

=item *

Data::Dumper

=back


=head1 INCOMPATIBILITIES

None reported.


=head1 BUGS AND LIMITATIONS

No bugs have been reported.

Please report any bugs or feature requests to
C<bug-class-std@rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org>.


=head1 AUTHOR

Damian Conway  C<< <DCONWAY@cpan.org> >>


=head1 LICENCE AND COPYRIGHT

Copyright (c) 2005, Damian Conway C<< <DCONWAY@cpan.org> >>. All rights reserved.

Portions of the documentation from "Perl Best Practices" copyright (c)
2005 by O'Reilly Media, Inc. and reprinted with permission.

This module is free software; you can redistribute it and/or
modify it under the same terms as Perl itself.


=head1 DISCLAIMER OF WARRANTY

BECAUSE THIS SOFTWARE IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY
FOR THE SOFTWARE, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN
OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES
PROVIDE THE SOFTWARE "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER
EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE
ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH
YOU. SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL
NECESSARY SERVICING, REPAIR, OR CORRECTION.

IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR
REDISTRIBUTE THE SOFTWARE AS PERMITTED BY THE ABOVE LICENCE, BE
LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL,
OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE
THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING
RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A
FAILURE OF THE SOFTWARE TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF
SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES.
