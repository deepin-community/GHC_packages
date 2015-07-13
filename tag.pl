#!/usr/bin/perl

# TODO:
#  * Check for uncommited changes before

use strict;
use warnings;

my @dirs = @ARGV;

unless (@dirs) {
	@dirs = glob 'p/*/';
}

my @to_build;

for my $dir (@dirs) {
	my $changelog = "$dir/debian/changelog";
	next unless -r $changelog;
	open CHANGELOG, '<', $changelog or die @!;
	my $firstline = <CHANGELOG>;
	if ($firstline =~ m/([\w-]+) \(([\w:~.+-]+)\) (\w+);/) {
		my ($source, $version, $suite) = ($1, $2, $3);
		my $tag = sprintf "%s_v%s", $source, $version;
		my $msg = sprintf "Tagging %s version %s, targetted for %s", $source, $version, $suite;
		if ($suite eq "UNRELEASED") {
			printf STDERR "Cannot tag UNRELEASED package %s-%s", $source, $version;
		} else {
			my $ret = system(qw/git tag -a -m/, $msg, $tag);
			die (sprintf "Failed to tag %s: %d\n", $tag, $?>>8) if $ret != 0;
			printf "Added tag %s\n", $tag;
		}
	} else {
		printf STDERR "Cannot parse %s:\n%s", $changelog, $firstline;
		next
	}
}



