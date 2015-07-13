#!/usr/bin/perl

use strict;
use warnings;

my @dirs = @ARGV;

unless (@dirs) {
	@dirs = glob 'p/*/';
}

my @to_build;

my %tags;
open TAGS, '-|', 'git tag -l' or die @!;
while (<TAGS>) { chomp; $tags{$_}++ };
close TAGS or die @!;

for my $dir (@dirs) {
	my $changelog = "$dir/debian/changelog";
	next unless -r $changelog;
	open CHANGELOG, '<', $changelog or die @!;
	my $firstline = <CHANGELOG>;
	if ($firstline =~ m/([\w-]+) \(([\w:~.+-]+)\) (\w+);/) {
		my ($source, $version, $suite) = ($1, $2, $3);
		my $tag = sprintf "%s_v%s", $source, $version;
		next if ($suite eq "UNRELEASED");
		next if ($tags{$tag});
		printf "%s\n", $dir;
	} else {
		printf STDERR "Cannot parse %s:\n%s", $changelog, $firstline;
		next
	}
}



