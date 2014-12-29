% BLOGLITERATELY(1) BlogLiterately Manual
% Robert Greayer, Brent Yorgey
% 2014-12-04
# NAME

BlogLiterately - post articles to blogs from command line

# SYNOPSIS

BlogLiterately \[OPTIONS\] FILE

# OPTIONS

-s,--style=FILE
:     style specification (for --hscolour-icss)
--hscolour-icss
:     highlight haskell: hscolour, inline style (default)
--hscolour-css
:     highlight haskell: hscolour, separate stylesheet
--hs-nohighlight
:     no haskell highlighting
--hs-kate
:     highlight haskell with highlighting-kate
--kate
:     highlight non-Haskell code with highlighting-kate (default)
--no-kate
:     don't highlight non-Haskell code
--no-toc
:     don't generate a table of contents (default)
--toc
:     generate a table of contents
-w,--wplatex
:     reformat inline LaTeX the way WordPress expects
-m,--math=ITEM
:     how to layout math, where --math=<pandoc-option>[=URL]
-g,--ghci
:     run [ghci] blocks through ghci and include output
-I,--upload-images
:     upload local images
-C,--category=ITEM
:     post category (can specify more than one)
-T,--tag=ITEM
:     tag (can specify more than one)
--blogid=ID
:     Blog specific identifier
-P,--profile=STRING
:     profile to use
-b,--blog=URL
:     blog XML-RPC url (if omitted, HTML goes to stdout)
-u,--user=USER
:     user name
-p,--password=PASSWORD
:     password
-t --title=TITLE
:     post title
-f --format=FORMAT
:     input format: markdown or rst
-i --postid=ID
:     Post to replace (if any)
--page
:     create a "page" instead of a post (WordPress only)
--publish
:    publish post (otherwise it's uploaded as a draft)
-h --html-only
:     don't upload anything; output HTML to stdout
--citations
:     process citations (default)
--no-citations
:     do not process citations
-x,--xtra=ITEM
:     extension arguments, for use with custom extensions
-?,--help
:     Display help message
-V,--version
:     Print version information
--numeric-version
:     Print just the version number

# NOTES

This manual page is written by Dmitry Bogatov for Debian project but
can be used by others under terms of GNU GPL version 3 or later.

# SEE ALSO

Tutorial and examples on project homepage [[http://byorgey.wordpress.com/blogliterately]]
