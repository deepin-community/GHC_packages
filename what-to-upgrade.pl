#!/usr/bin/perl

use strict;
use warnings;

use File::Slurp;

my @dirs = @ARGV;

unless (@dirs) {
	@dirs = glob 'p/*/';
}

my %plan;
open PLAN, '<', '../package-plan/packages.txt' or die @!;
while (<PLAN>) {
        chomp;
        next if /^#/;
        next if /^\s*$/;
        unless (m/^(.*?) (.*?)(?: ([^#]*))?(?:#.*)?$/) {
                print "Ignoring unparseable line $.: $_\n";
        }
        my ($pkg,$version,$attribs) = ($1,$2,$3);
	$plan{$pkg} = $version;
}
close PLAN or die @!;

for my $dir (@dirs) {
	my $changelog = "$dir/debian/changelog";
	my $watchfile = "$dir/debian/watch";
	next unless -r $changelog;
	next unless -r $watchfile;

	open CHANGELOG, '<', $changelog or die @!;
	my $firstline = <CHANGELOG>;
	if ($firstline =~ m/([\w-]+) \(([\w:~.+-]+)\) (\w+);/) {
		my ($source, $version, $suite) = ($1, $2, $3);
		my ($cabal_version) = ($version =~ m/^(?:.*:)?(.*?)(?:\+dfsg\d*)?-.*?$/);

		my $watch = read_file($watchfile) or die @!;
		unless ($watch =~ m!http://hackage.haskell.org/package/(.*)/distro-monitor!) {
			#printf STDERR "Cannot parse watchfile %s\n", $watchfile;
			# ignore packages with non-standard watch files
			next
		}
		my $cabal_package = $1;

		unless (exists $plan{$cabal_package}) {
			printf STDERR "W: package %s not in the package plan\n",
				$dir, $cabal_package;
			next
		}

		if ($cabal_version ne $plan{$cabal_package}) {
			printf "%s\n", $dir;
		}
	} else {
		printf STDERR "Cannot parse %s:\n%s", $changelog, $firstline;
		next
	}
}



