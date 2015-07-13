#!/usr/bin/perl

use strict;
use Dpkg::Control;
use Dpkg::Control::Info;
use Data::Dump qw/dump/;


unless (scalar @ARGV > 0) {
	print <<__END__;
Usage: $0 <dir / control / dsc>...

Each argument is expected to be a Debian source package directory; the debian
directory in a Debian source package directory the control file or the .dsc
file of a Debian source. These will be ordered by “obvious” build-dependencies
and printed out again.
__END__
	exit;
}

my %builds;
my %depends;

sub parse_control {
	my $dir=shift;
	my $control=shift;

	my $data = new Dpkg::Control::Info ($control);
	my $src = $data->get_source();
	$depends{$dir} = [];
	push @{$depends{$dir}}, $_
		for $src->{"Build-Depends"} =~ /([\w\d-_]+)(?:\s+\(.*?\))?\s*,?/sg;

	$builds{$_->{Package}} = $dir
		for $data->get_packages();
}

sub parse_dsc {
	my $dir=shift;
	my $dsc=shift;

	my $data = new Dpkg::Control ((type => CTRL_PKG_SRC));
	$data->load($dsc);

	$depends{$dir} = [];
	push @{$depends{$dir}}, $_
		for $data->{"Build-Depends"} =~ /([\w\d-_]+)(?:\s+\(.*?\))?\s*,?/sg;

	$builds{$_} = $dir
		for $data->{"Binary"} =~ /([\w\d-_]+)\s*,?/sg;
}

for my $dir (@ARGV) {
	if (-f "$dir/debian/control") {
		parse_control($dir, "$dir/debian/control");
		next;
	}
	if (-f "$dir/control" ) {
		parse_control($dir, "$dir/control");
		next;
	}
	if ($dir =~ m/control$/ and -f $dir) {
		parse_control($dir, $dir);
		next;
	}
	if ($dir =~ m/\.dsc$/ and -f $dir) {
		parse_dsc($dir, $dir);
		next;
	}
	warn "Could not find a control file for argument $dir\n";
}

my %waiting; 
my %done; 

sub do_pkg {
	my $dir = shift;
	return if $done{$dir};
	return if $waiting{$dir}; # loop breaker
	$waiting{$dir}++;
	for my $deb (@{$depends{$dir}}) {
		if (exists $builds{$deb}) {
			do_pkg ($builds{$deb});
		}
	}
	print "$dir\n";
	delete $waiting{$dir};
	$done{$dir} ++;
}

do_pkg ($_) for @ARGV;
