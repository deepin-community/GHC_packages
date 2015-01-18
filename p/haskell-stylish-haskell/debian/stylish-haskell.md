% STYLISH-HASKELL
% Jasper Van der Jeugt <m@jaspervdj.be>
% 2015-01-16
# NAME

stylish-haskell -- simple Haskell code prettifier.

# SYNOPSIS

stylish-haskell --inplace \[OPTIONS] FILE [FILES]

stylish-haskell \[OPTIONS]

# DESCRIPTION

Simple Haskell code prettifier. This tool tries to help where
necessary without getting in the way.

It can change files inplace, if *--inplace* option is used,
read stdin and output stdout otherwise.

For more detailed configuration, see *--defaults* option.

#OPTIONS

-c --config=FILE
:    Configuration file
-v --verbose
:    Run in verbose mode
-d --defaults
:    Dump default config and exit
-i --inplace
:    Overwrite the given files in place
-? --help
:    Display help message
-V --version
:    Print version information
