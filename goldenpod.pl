#!/usr/bin/perl
# GoldenPod
# Copyright (C) Eskild Hustvedt 2005, 2006, 2007, 2009, 2010, 2011
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# Force strict mode, and useful warnings
use strict;
use warnings;
use Carp qw( croak );

# Require perl 5.14 to get unicode_strings, say, //, package blocks
#use 5.014;
# Activate the use of state variables
use feature qw(state switch);
# Make perl automatically die if any of these fails
use Fatal qw( open chdir mkdir close );
# Used to find our directory and name
use File::Basename;
# Used to create our dirs
use File::Path qw(mkpath);
# Digest md5 for hashing the urls
use Digest::MD5 qw(md5_hex);
# We need realpath and getcwd
use Cwd qw(getcwd realpath);
# Copying files
use File::Copy;
# Used to get OS name for user agent and for date
use POSIX qw(uname strftime mktime);
# open() call to commands
use IPC::Open2;
# Get the switch instruction
#use Switch 'Perl6';
# Parse the feeds as xml
use XML::Simple;
# Parsing of commandline parameters
use Getopt::Long;
# Allow bundling of options with GeteOpt
Getopt::Long::Configure ('bundling', 'prefix_pattern=(--|-)');

#XXX: my $ARGV_ = \@ARGV; 
# Using true/false is easier to read than 0/1
use constant { true => 1, false => 0 };
# Version number:
use constant VERSION => '0.9-M1ch';


###############################################################################
#Global Variables:
###############################################################################
# Version number
# my $Version = '0.9-M1ch';

# Hash for the global configuration
my $config_ = {
  verbose           => 0,
# When set to any true value this disables logging. When set to 2 it
# disables logging and directs all of our output to /dev/null
  no_log            => 0,
# When set to true, goldenpod will only download the first entry in a feed
  first_only        => 0,
# When set to true, goldenpod will not download anything at all, but will
# add all entries in all feeds to its list of already downloaded files.
  no_download_mode  => 0,
# When set to true, goldenpod will delete files when in --copy mode
  copy_files_delete => 0,
# If true, enables dry-run mode
  dry_run_mode      => 0,
# A regex used to ignore files, either from the config or --ignore-pattern
  ignore_patter     => undef,
# Where to copy files to when in --copy mode
  copy_files_to     => undef,
# True if we should delete old podcasts
  remove_old_files  => 0,
# Maximum age of Podcast in Days (0=infinite)
  max_age           => 0,
# Name of the Podcast
  podcast_name      => '',
# The number of --files to --copy or --rmold
  file_number       => undef,
  
  default_verbosity => 0,
  
  working_dir       => undef,
  
  podcast_filter    => undef,
  
  ignore_patter_glob=> undef,
  
  commands          => {},
# Hash for the global configuration  
  podcasts          => {
    downloaded => {},
    feeds      => {},
    num        => 0,
  },
  
  audio_regex       => '(ogg|oga|mp3|m4a|wave?|flac|wma|ape|tta|aac|mp2|mpa|ram?|aiff?|au|mpu)',
# The global date, as used in directory names, value set in main()  
  date              => '',
# The downloader  
  downloader        => 'LWP',
# Available options  
  has               => {
    LWP   => 0,
    LWPS  => 0,
    curl  => 0,
    HTMLE => undef
  },
# The user agent string  
  user_agent        => ('GoldenPod/'.VERSION.' (%OS%; podcatcher; Using %downloader%)'),
# Salt for the creation of the hashes of the podcast url
  hash_salt => "42",
# Set to 1 after the main config is loaded  
  config_loded => 0,
# Set to 1 after the podcast config is loaded  
  podcasts_loded => 0,
# Set to 1 after the podcast log is loaded  
  log_loded => 0,
};

# Hash for the global configuration
my $podcasts_ = {
  downloaded => {},
  feeds      => {},
  num        => 0};

# all podcast news feeds in one hash  
my $feeds_ = {};


# List of all feeds from the podcasts.conf file
#my $FeedList=[];

# Hash for the global configuration
#my $PODCAST_CONFIG = {};


# Return values that are captured by our SIGCHLD handler
my %ReturnVals;
# Contains a list of already downloaded podcasts
#my %AlreadyDownloaded;
# Contains a list of podcasts to be added to our "already downloaded" list
my %NoDownload;
# Podcast feed names used to populate the catalogue
#my %PodNames;

# Our user agent string
#my $UserAgent = 'GoldenPod/'.$Version.' (%OS%; podcatcher; Using %downloader%)';

# These are global state varibles for the LWP download component
my $lastP = 0;
my $prevLen = 0;

# This makes sure children are slayed properly and their return values
# are kept (in the %ReturnVals hash)
$SIG{CHLD} = sub
{
  my $PID = wait;
  $ReturnVals{$PID} = $? >> 8;
  return(1);
};

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parse command-line options
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parse commandline arguments
GetOptions (
  'version'                      => sub{ $config_->{commands}->{version} = 1 },
  'help|h'                       => sub{ $config_->{commands}->{help} = 1 },
  'help-all'                     => sub{ $config_->{commands}->{help_all} = 1 },
  'clean'                        => sub{ $config_->{commands}->{clean} = 1 },
  't|list|listfeeds'             => sub{ $config_->{commands}->{list} = 1 },
  # Display some simple statistics
  'stats'                        => sub{ $config_->{commands}->{stats} = 1 },
  # Display information about which config files and settings we would use
  'dumpinfo|debuginfo'           => sub{ $config_->{commands}->{dumpinfo} = 1 },
  'rmold|o'                      => sub{ $config_->{commands}->{rmold} = 1 },

  'a|add|addfeed=s'              => sub{ $config_->{commands}->{add} = $_[1] },
  'r|remove|rmfeed|removefeed=s' => sub{ $config_->{commands}->{remove} = $_[1] },
  'fuzzydump=s'                  => sub{ $config_->{commands}->{fuzzydump} = $_[1] },
  'rssdump=s'                    => sub{ $config_->{commands}->{rssdump} = $_[1] },
  'ping=s'                       => sub{ $config_->{commands}->{ping} = $_[1] },
  'quick=s'                      => sub{ $config_->{commands}->{quick} = $_[1] },
  'copy|c=s'                     => sub{ $config_->{commands}->{copy} = $_[1] },

  'age|maxage=s'                     => \$config_->{max_age},
  'n|files=s'                        => \$config_->{file_number},
  'ignore-pattern|ignorepattern|i=s' => \&ignorePattern,
  
  'first-only|firstonly|first|f'     => \$config_->{first_only},
  'name=s'                           => \$config_->{podcast_name},
  'silent|s'                         => \&setSilentMode,
  'delete|d'                         => \$config_->{copy_files_delete},
  'verbose|v'                        => sub { argVerbose() },
  'no-download|nodownload|w'         => sub { argVerbose(); $config_->{no_download_mode} = 1 },
  'dry-run|dryrun|u'                 => sub { argVerbose(); $config_->{dry_run_mode} = 1},
  'nolog|l'                          => sub { $config_->{no_log} = 2},
  'prefer-curl|prefercurl'           => sub { $config_->{downloader} = 'curl' }
  
) or croak 'Run ', basename($0), " --help for help\n";


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# help function declerations
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: Print formatted --help output
# Usage: printHelp('-shortoption', '--longoption', 'description');
#  Description will be reformatted to fit within a normal terminal
sub printHelp
{
  # The short option
  my $short = shift,
  # The long option
  my $long = shift;
  # The description
  my $desc = shift;
  # The generated description that will be printed in the end
  my $GeneratedDesc;
  # The current line of the description
  my $currdesc = '';
  # The maximum length any line can be
  my $maxlen = 80;
  # The length the options take up
  my $optionlen = 20;
  # Check if the short/long are LONGER than optionlen, if so, we need
  # to do some additional magic to take up only $maxlen.
  # The +1 here is because we always add a space between them, no matter what
  if ((length($short) + length($long) + 1) > $optionlen)
  {
    $optionlen = length($short) + length($long) + 1;
  }
  # Split the description into lines
  foreach my $part (split(/ /,$desc))
  {
    if(defined $GeneratedDesc)
    {
      if ((length($currdesc) + length($part) + 1 + 20) > $maxlen)
      {
        $GeneratedDesc .= "\n";
        $currdesc = '';
      }
      else
      {
        $currdesc .= ' ';
        $GeneratedDesc .= ' ';
      }
    }
    $currdesc .= $part;
    $GeneratedDesc .= $part;
  }
  # Something went wrong
  croak('Option mismatch') if not $GeneratedDesc;
  # Print it all
  foreach my $description (split(/\n/,$GeneratedDesc))
  {
    printf "%-4s %-15s %s\n", $short,$long,$description;
    # Set short and long to '' to ensure we don't print the options twice
    $short = '';$long = '';
  }
  # Succeed
  return true;
}

# Purpose: Output the program --help info.
# Usage: help();
sub help
{
#  $config_->{podcast_log} =~ s/$ENV{HOME}/~/;
  my $all = shift;
  printVersion();
  printf("\nUsage: %s", basename($0));
  printf("\n  or : %s {[COMMAND] [OPTIONS]}\n\n", basename($0));
  printf("COMMANDS:\n");
  printHelp('', '--version', 'Display version information and exit');
  printHelp('-h', '--help', 'This help screen');
  printHelp('', '--help-all', 'Print an extended help screen with additional options');
  printHelp('','--clean','Clean up the podcasts directory and catalogue.');
  printHelp('', '--list', 'Print the list of podcasts added to goldenpod');
  printHelp('', '--stats', 'Print some simple statistics');
  printHelp('-o', '--rmold', 'Delete N old podcasts where N is 4 or the number supplied to --files');
  printHelp('-a', '--add [URL]', 'Add the URL specified as a feed in goldenpod. You may optionally supply a second parameter, which makes the feed use the fuzzy parser with the regular expression supplied.');
  printHelp('-r', '--remove [URL]', 'Remove the URL specified from goldenpod\'s feed list.');
  printHelp('', '--ping URL', 'Test URL using both parsers and display which parser is recommended for it.');
  printHelp('-c', '--copy [path]', 'Copy the last N downloaded files to path. N is either 4 or the number supplied to --files.');
  printHelp('','--quick','Download the first podcast found in the feed supplied to this parameter and exit');
  if ($all)
  {
    printHelp('', '--debuginfo', 'Print the files goldenpod works with and some system information');
    printHelp('', '--fuzzydump URL', 'Dump the list of files found by the fuzzy parser in URL. See the manpage for more information');
    printHelp('', '--rssdump URL', 'Dump the list of files found by the standard parser in URL. See the manpage for more information');
  }
  printf("\nOPTIONS:\n");
  # Inform the user about the default based upon the value of $config_->{default_verbosity}
  if ($config_->{default_verbosity})
  {
    printHelp('-v', '--verbose', 'Be verbose (default)');
    printHelp('-s', '--silent', 'Be silent');
  }
  else
  {
    printHelp('-v', '--verbose', 'Be verbose');
    printHelp('-s', '--silent', 'Be silent (default)');
  }
  printHelp('-u', '--dry-run', 'Display what would be done but don\'t do it. Implies --verbose');
  printHelp('-w', '--no-download','Mark all podcasts as downloaded. Implies --verbose.');
  printHelp('-f', '--first-only', 'Only download the first file in any feed. Permanently ignore the others.');
  printHelp('-n', '--files N', 'Copy N files instead of 4 (use with --copy or --rmold)');
  printHelp('-d', '--delete', 'Delete all other files in the target --copy directory');
  printHelp('-i', '--ignore-pattern', 'Ignore filenames matching the regexp pattern supplied when downloading, copying or deleting podcasts.');
  if ($all)
  {
    printHelp('-l', '--nolog', 'Don\'t create a message logfile when in silent mode.');
    printHelp('', '--prefer-curl', 'Prefer to use curl for downloading if available, instead of LWP (curl is used by default if LWP is missing)');
  }
}

# Purpose: Print version and warranty information
# Usage: printVersion();
sub printVersion {
  printf( "GoldenPod %s\n", VERSION);
  return 0;
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Download helpers
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# GPLWP is a subclass of LWP::UserAgent that overrides the progress method in
# order to output more useful progress messages from goldenpod.
package GPLWP
{
  our @ISA = qw(LWP::UserAgent);
  sub progress
  {
    shift;
    no warnings 'once';
    return if not $main::Verbose;
    my ($status, $response) = @_;
    $| = 1;
    if ($status eq 'tick')
    {
      main::progressIndicator();
    }
    elsif ($status eq 'begin')
    {
      main::progressIndicator();
    }
    elsif ($status eq 'end')
    {
      return;
    }
    else
    {
      $status = int($status * 100);
      main::iPrint(sprintf('%-4s',$status.'%'));
    }
  }
}

# Purpsoe: Initialize the downloader
# Usage: initDownload();
sub initDownload
{
  state $downloadInitialized = 0;
  if ($downloadInitialized)
  {
    return;
  }
  detectDownloader();
  my $os = [uname()]->[0];
  $os = $os eq 'Linux' ? 'GNU/Linux' : $os;
  $config_->{user_agent} =~ s/%OS%/$os/g;
  $config_->{user_agent} =~ s/%downloader%/$config_->{downloader}/g;
  $downloadInitialized = 1;
}

# Purpose: Detect our downloader
# Usage: detectDownloader();
sub detectDownloader {
  inPath('curl') and $config_->{has}->{curl} = 1;
  eval {require LWP::UserAgent} and $config_->{has}->{LWP} = 1;
  eval {require LWP::UserAgent::https} and $config_->{has}->{LWPS} = 1;

  if(not $config_->{has}->{LWP} and not $config_->{has}->{curl}) {
    croak("No downloader found. GoldenPod needs either LWP (libwww-perl) or curl.\n");
  }  

  #Auto select downloader
  if( ($config_->{downloader} eq 'curl' or not $config_->{has}->{LWP}) 
      and $config_->{has}->{curl}) {
    $config_->{downloader} = 'curl';
  }
  return;
}

# Purpose: Fetch a URL, either returning the data or writing a file
# Usage: fetchURL( SOME_URL, FileName?);
# Will download to FileName if present, if undef then it will return
# the content;
sub fetchURL
{
  # Initialize the downloader if needed
  initDownload();

  my $URL = shift;
  my $targetFile = shift;

  if(not $URL =~ m#^\S+://#)
  {
    $URL = 'http://'.$URL;
  }
  
  my $downloader = $config_->{downloader};
  if( $URL =~ m#^https://#) {
    if($downloader eq 'LWP' and not $config_->{has}->{LWPS}) {
      if( $config_->{has}->{curl}) {
        $downloader = 'curl';
      }
      else {
        croak "https is currently not supportet. install curl, or LWP::Protocol::https.\n"
      }
    }
  }
  
  if ($downloader eq 'LWP')
  {
    # Reset our state
    $lastP = 0;
    $prevLen = 0;

    # GPLWP is a GoldenPod wrapper around LWP
    my $UA = GPLWP->new(
      agent => $config_->{user_agent}.' libwwwperl',
      requests_redirectable => [ 'GET', 'HEAD' ],
    );
    # Honor proxy settings in env
    $UA->env_proxy();
    my $response;

    # if we have a target file then we just use ->mirror, that downloads
    # it to a file instead and handles all the nasties for us.
    if ($targetFile)
    {
      printVerbose('Downloading '.$URL.' ... ');
      $response = $UA->mirror($URL,$targetFile);
    }
    # If we don't, just use standard get
    else
    {
      printVerbose('Fetching '.$URL.' ... ');
      $response = $UA->get($URL);
    }
    if ($config_->{verbose})
    {
      iPrint('100% done');
      printVerbose ("\n");
    }
    if( $response->is_success)
    {
      # Return the content
      return $response->content;
    }
    
    warn("Download of $URL failed: ".$response->status_line."\n");
    if( $config_->{has}->{curl}){
      # Try to download using curl if https is missing
      $downloader = 'curl';
    }
    else{
      return;
    }
  }
  
  if ($downloader eq 'curl')
  {
    my ($Child_IN, $Child_OUT, $Output);
    # Curl options:
    # -C to continue when possible
    # -k for insecure (allow ssl servers with odd certificates)
    # -L to follow location hints
    # -A sets the user agent string
    my @CurlArgs = ( qw(-C - -k -L -A),$config_->{user_agent});

    # Set verbosity
    if ($config_->{verbose})
    {
      push(@CurlArgs,'-#');
    }
    else
    {
      push(@CurlArgs, qw(--silent --show-error));
    }
    my $FILE;
    # Output to a file
    if ($targetFile)
    {
      open( $FILE, '>', $targetFile) or 
        croak( "Cannot open " . $targetFile . " for write\n");
      binmode($FILE);
  
      printVerbose('Downloading '.$URL."\n");
    }
    else
    {
      printVerbose('Fetching '.$URL."\n");
    }

    my $PID = open2($Child_OUT, $Child_IN, 'curl',@CurlArgs,$URL) or croak("Unable to open3() connection to curl: $!\n");
#curl -C - -k -L -A GoldenPod/2 (GNU/Linux; podcatcher; Using curl) -# 
    # Read from curl
    while(<$Child_OUT>)
    {
      if ($targetFile) {
        # Write direct to outputfile 
        #TODO: write only each X MB
        print( $FILE $_);
      }
      else {
        $Output .= $_;
      }
    }
    close($Child_OUT);
    close($Child_IN);
    
    if ($targetFile) {
      close($FILE);
    }
    
    # If we don't have a return value yet, wait one second and see if we get one
    if(not defined $ReturnVals{$PID})
    {
      sleep(1);
    }

    if(defined $ReturnVals{$PID} and not $ReturnVals{$PID} == 0)
    {
      warn("Download of $URL failed, curl exited with the return value ".$ReturnVals{$PID}."\n");
      return;
    }
    # Return the output
    return $Output;
  }
  else
  {
    croak("Unknown downloader: $config_->{downloader}\n");
  }
}

# Purpose: Download a URL to a file
# Usage: downloadToFile(ToDir, URL);
#  Also handles creating the ToDir
sub downloadToFile  {
  my ($dir, $file, $URL) = @_;
  if(not -d $dir)  {
    mkdir($dir) or croak("Unable to mkdir $dir: $!\n");
  }

  my $abs_file_name = $dir . '/' . $file;

  if(fetchURL($URL,$abs_file_name))  {
    return true;
  }
  else  {
    return false;
  }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Helper functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: A print that removes previous text before printing. Used for status Information messages.
# Usage: iPrint(text);
sub iPrint {
  local $| = false;
  my $data = shift;
  if ($prevLen)  {
    for(my $i = 0; $i < $prevLen; $i++) {
      print "\b \b";
    }
  }
  $prevLen = length($data);
  print $data;
  local $| = true;
}

# Purpose: Output pretty progression indicator
# Usage: progressIndicator();
#    - Outputs something every ten times it is called
sub progressIndicator {
  state $state = 0;
  my @states = ['-', '\\', '|', '/'];
  iPrint( $states[$state]);
  
  $state = (++$state) % 4;
  
  return true;
}

# Purpose: print() something if we're verbose
# Usage: printVerbose(OPTS);
#  OPTS are identical to print();
sub printVerbose {
  if($config_->{verbose}) {
    print(@_);
  }
}

# Purpose: Get the path to the logfile
# Usage: logfile = getLogFile();
sub getLogFile {
  return $config_->{working_dir}.'/goldenpod.log';
}

# Purpose: Prepare logging (if needed)
# Usage: prepareLogging();
sub prepareLogging {
  if(not $config_->{no_log})  {
    my $ProgramLog = getLogFile();
    # Unless we're verbose, write stuff to $ProgramLog
    open(STDOUT, '>>',$ProgramLog);
    open(STDERR, '>>',$ProgramLog);
    # Log the date and time we started
    print 'Started at ' . localtime(time);
  }
  # If we're not verbose and $config_->{no_log} is 2 (-l) then write stuff to /dev/null ;)
  elsif($config_->{no_log} == 2)  {
    open(STDOUT, '>', '/dev/null');
    open(STDERR, '>', '/dev/null');
  }
}

# Purpose: Check for a file in path
# Usage: inPath(FILE)
sub inPath {
  foreach (split /:/, $ENV{PATH}) { if (-x "$_/@_" and ! -d "$_/@_" ) {  return "$_/@_"; } } return false;
}

# Purpose: Check if a directory is empty
# Usage: disIsEmpty(PATH);
#  Returns 1 if it is empty, 0 if it isn't.
sub disIsEmpty {
  my $dir = shift;
  opendir(TESTDIR, $dir);
  my @TestDir = readdir(TESTDIR);
  closedir(TESTDIR);
  if(not scalar @TestDir > 2) {
    return true;
  }
  return false;
}

# Purpose: Prefix a "0" to a number if it is only one digit.
# Usage: my $NewNumber = prefixZero(NUMBER);
sub prefixZero {
  my $number = shift;
  if ($number =~ /^\d$/)  {
    return("0$number");
  }
  return($number);
}

# Purpose: Get OS/distro version information
# Usage: print "OS: ",getDistVer(),"\n";
sub getDistVer {
  # Try LSB first
  my %LSB;
  if (-e '/etc/lsb-release'){
    loadConfigFile('/etc/lsb-release',\%LSB);
    if(defined($LSB{DISTRIB_ID}) and $LSB{DISTRIB_ID} =~ /\S/ and defined($LSB{DISTRIB_RELEASE}) and $LSB{DISTRIB_RELEASE} =~ /\S/)  {
      my $ret = '/etc/lsb-release: '.$LSB{DISTRIB_ID}.' '.$LSB{DISTRIB_RELEASE};
      if(defined($LSB{DISTRIB_CODENAME})) {
        $ret .= ' ('.$LSB{DISTRIB_CODENAME}.')';
      }
      return($ret);
    }
  }
  # GNU/Linux and BSD
  foreach(qw/arch mandriva mandrakelinux mandrake fedora redhat red-hat ubuntu debian gentoo suse distro dist slackware freebsd openbsd netbsd dragonflybsd NULL/)  {
    if (-e "/etc/$_-release" or -e "/etc/$_-version" or -e "/etc/${_}_version" or $_ eq 'NULL') {
      my ($DistVer, $File, $VERSION_FILE);
      if(-e "/etc/$_-release") {
        $File = "$_-release";
        open($VERSION_FILE, '<', "/etc/$_-release");
        $DistVer = <$VERSION_FILE>;
      } elsif (-e "/etc/$_-version") {
        $File = "$_-version";
        open($VERSION_FILE, '<', "/etc/$_-release");
        $DistVer = <$VERSION_FILE>;
      } elsif (-e "/etc/${_}_version") {
        $File = "${_}_version";
        open($VERSION_FILE, '<', "/etc/${_}_version");
        $DistVer = <$VERSION_FILE>;
      } elsif ($_ eq 'NULL') {
        last unless -e '/etc/version';
        $File = 'version';
        open($VERSION_FILE, '<', '/etc/version');
        $DistVer = <$VERSION_FILE>;
      }
      close($VERSION_FILE);
            $DistVer //= '';
      chomp($DistVer);
      return("/etc/$File: $DistVer");
    }
  }
  # Didn't find anything yet. Get uname info
  my ($sysname, $nodename, $release, $version, $machine) = POSIX::uname();
  if ($sysname =~ /darwin/i) {
    my $DarwinName;
    my $DarwinOSVer;
    # Darwin kernel, try to get OS X info.
    if(inPath('sw_vers')) {
      if(eval {require IPC::Open2} ) {
        if(open2(my $SW_VERS, my $NULL_IN, 'sw_vers')) {
          while(<$SW_VERS>) {
            chomp;
            if (s/^ProductName:\s+//gi) {
              $DarwinName = $_;
            } elsif(s/^ProductVersion:\s+//) {
              $DarwinOSVer = $_;
            }
          }
          close($SW_VERS);
        }
      }
    }
    if(defined($DarwinOSVer) and defined($DarwinName)) {
      return("$DarwinName $DarwinOSVer ($machine)");
    }
  }
  # Detect additional release/version files
  my $RelFile;
  foreach(glob('/etc/*'))
  {
    next if not /(release|version)/i;
    next if m/\/(subversion|lsb-release)$/;
    if ($RelFile)
    {
      $RelFile .= ', '.$_;
    }
    else
    {
      $RelFile = ' ('.$_;
    }
  }
  if ($RelFile)
  {
    $RelFile .= ')';
  }
  else
  {
    $RelFile = '';
  }
  # Some distros set a LSB DISTRIB_ID but no version, try DISTRIB_ID
  # along with the kernel info.
  if ($LSB{DISTRIB_ID})
  {
    return($LSB{DISTRIB_ID}."/Unknown$RelFile ($sysname $release $version $machine)");
  }
  return("Unknown$RelFile ($sysname $release $version $machine)");
}

# Purpose: Display useful information
# Usage: dumpInfo();
sub dumpInfo
{
  initialize('util');
  version();
  print "\n";
  my $pattern = "%-28s: %s\n";
  printf($pattern, 'Configuration file',$config_->{user_config_dir}.'/goldenpod.conf');
  printf($pattern, 'Podcast list',$config_->{podcast_list});
  if ($config_->{verbose} and not $config_->{no_log})
  {
    printf($pattern, 'Logfile',getLogFile());
  }
  printf($pattern, 'List of downloaded podcasts',$config_->{podcast_log});
  printf($pattern, 'Target download directory',$config_->{working_dir});
  printf($pattern, 'Perl version', sprintf('%vd',$^V));
  # Don't output useless "used only once, possible typo" warnings
  no warnings 'once';
  if (eval {require LWP} and eval {require LWP::UserAgent})
  {
    printf($pattern,'LWP version',$LWP::VERSION);
    printf($pattern,'LWP::UserAgent version',$LWP::UserAgent::VERSION);
  }
  else
  {
    printf($pattern,'LWP','missing');
  }
  my $HTMLEV = 'missing';
  if(eval {require HTML::Entities} ) {
    $HTMLEV = $HTML::Entities::VERSION;
  }
  printf($pattern,'HTML::Entities',$HTMLEV);
  printf($pattern, 'OS',getDistVer());
  eval {require Digest::MD5};
  my $md5 = Digest::MD5->new();
  my $self = $0;
  if(not -f $self)
  {
    $self = inPath($self);
  }
  open(my $f,'<',$self);
  $md5->addfile($f);
  my $digest = $md5->hexdigest;
  close($f);
  printf($pattern,'MD5',$digest);
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# NoDownload routine
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: Adds all items in %NoDownload to the logfile
# Usage: performNoDownload();
sub performNoDownload
{
  if(not $config_->{first_only})
  {
    printVerbose("\nWriting older podcasts to the logfile...");
  }
  open(my $LOGFILE, '>>',$config_->{podcast_log});
  foreach (keys (%NoDownload))
  {
    if(not $podcasts_->{downloaded}->{$_})
    {
      print $LOGFILE $_."\n";
    }
  }
  if(not $config_->{first_only})
  {
    printVerbose(" done\n");
  }
  close($LOGFILE);
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Helper routines for --copy and --rmold
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: Returns the value supplied to --files or 4
# Usage: howManyFiles();
sub howManyFiles
{
  my $FileCount = $config_->{file_number} ? $config_->{file_number} : 4;
  # Unless it's an integer we can't continue
  if ($FileCount =~ /\D/)
  {
    croak "Error: The option passed to --files ($FileCount) is not an integer number.\n";
  }
  return $FileCount;
}

# Purpose: Finds and sorts files in the current directory by time (newest first)
#  and returns an array containing them.
# Usage: my @List = sortedFileList();
# NOTE: Ignores files that aren't symlinks.
sub sortedFileList
{
  my $FromDir = shift;
  my (@sortedFileList, %FileCopyList);

  # For compatibility with older GoldenPods
#  my $CWD = getcwd();
#  chdir($FromDir);
  
  my @file_list = glob($FromDir . "/*");
  
  # Create a hash of possible filenames
  foreach my $FileName (@file_list) {
    # Is it a link? If it isn't then we don't bother testing it
    next if not -l $FileName;
    # If the link points to something that doesn't exist them we omit it.
    if (-e (readlink $FileName)) {
      # We don't care about directories
      if(not -d $FileName) {
        $FileCopyList{Cwd::realpath(readlink($FileName))} = 1;
      }
    }
  }
  # Create a sorted array of filenames
  # map { [ $_, -M $_||0 ] } keys(%FileCopyList); = that means make a two dimensional array so that $_->[0]="filename.txt" and $_->[1] is the -M value
  # sort { $a->[1] <=> $b->[1] } = sort the array we just thought of by the $_->[1] index, ie the -M times
  # map { $_->[0] } take the sorted array and convert it back to a plain list of filenames by reading out the $_->[0] values.  These are now in the right order as they are sorted
  @sortedFileList = map { $_->[0] } sort { $a->[1] <=> $b->[1] } map { [ $_, -M $_ ] } keys(%FileCopyList);
#  chdir($CWD);
  return @sortedFileList;
}

# Purpose: Clean up the catalogue.
# Usage: cleanCatalogue();
sub cleanCatalogue {
  my $CatalogueBase = shift;
  croak("Catalogue didn't exist. Maybe you haven't downloaded anything yet?\n") if not -e $CatalogueBase;
  # For compatibility with older GoldenPods
  my $CWD = getcwd();
  chdir($CatalogueBase);
  my $removedSomething = false;
  print 'Cleaning up the catalogue...';
  
  my @base_element = glob("$CatalogueBase/*");
  
  foreach (@base_element) {
    print ".\n";
    next if not -d $_;
    
    print "1\n";
  }
  
  foreach my $CurrentDirectory (glob("$CatalogueBase/*")) {
    chdir($CurrentDirectory);
    foreach my $CurrentFile (glob("$CurrentDirectory/*")) {
      next if not -l $CurrentFile; # check if link
      if(not -e readlink($CurrentFile)) {
        $removedSomething = true;
        unlink($CurrentFile);
      }
    }
    chdir('..');
  }
  chdir($CWD);
  if (not $removedSomething) {
    print "nothing to clean\n";
  }
  else {
    print "done\n";
  }
}

# Purpose: Remove empty directories in ./
# Usage: removeEmptyDirs();
sub removeEmptyDirs
{
  my $FromDirectory = shift;
  my $removed = 0;
  foreach my $Directory (glob("$FromDirectory/*")) {
    next if not -d $Directory;
    if(disIsEmpty($Directory)) {
      rmdir($Directory);
      $removed++;
    }
  }
  return $removed;
}

# Purpose: Rewrite all playlists
# Usage: rewritePlaylists(Base CatalogueDir);
#  Call this before you delete directories to remove old playlists that are no
#  longer used.
sub rewritePlaylists
{
  my $CatalogueBase = shift;
  print 'Rewriting playlists...';
  my $CWD = getcwd();
  foreach (glob("$CatalogueBase/*"))
  {
    # Only process directories and don't process 'All'
    if(not -d $_ or basename($_) eq 'All')
    {
      next;
    }
    # This variable will be 1 if we wrote something to the playlist
    my $WrotePlaylistContent = 0;
    # The dirname, as used in the playlist filenames
    my $DirName = basename($_);
    if (-e $_.'/'.$DirName.'m3u')
    {
      unlink($_.'/'.$DirName.'.m3u');
    }
    # Skip directory if it's empty
    if(disIsEmpty($_))
    {
      next;
    }
    # Open our new playlist
    open(my $PLAYLIST, '>', "$_/$DirName.m3u");
    chdir($_);
    # Create the playlist based upon the output of sortedFileList
    foreach my $CurrentFile (sortedFileList($_))
    {
      # Skip playlists
      next if $CurrentFile =~ /\.m3u$/;
      # Add to the playlist
      print $PLAYLIST basename($CurrentFile),"\n";
      $WrotePlaylistContent = 1;
    }
    close($PLAYLIST);
    if(not $WrotePlaylistContent)
    {
      unlink($_.'/'.$DirName.'.m3u');
    }
    chdir($CWD);
  }
  print "done\n";
}

# Purpose: Clean up our directories
# Usage: cleanupDirs();
sub cleanupDirs {
  initialize('util');
  # Flush the output buffer faster.
  $| = 1;
  # Set the path to the Catalogue
  my $CatalogueDirectory = $config_->{working_dir} . "/catalogue/";
  # Remove dead links in the catalogue
  cleanCatalogue($CatalogueDirectory);
  # Rewrite the playlists, removing the dead files
  rewritePlaylists($CatalogueDirectory);
  # Remove empty directories
  print 'Removing empty directories...';
  my $removed = 0;
  $removed += removeEmptyDirs($CatalogueDirectory);
  $removed += removeEmptyDirs($config_->{working_dir});
  if ($removed) {
    print "done\n";
  }
  else {
    print "nothing to remove\n";
  }
  if(-e ($config_->{working_dir}.'/latest') and not readlink($config_->{working_dir}.'/latest')) {
    unlink($config_->{working_dir}.'/latest');
  }
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subroutines for --copy and --rmold
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: Delete old podcasts
# Usage: deleteOldPodcasts();
sub deleteOldPodcasts {
  # Conflicting commandline arguments
  if ($config_->{first_only} and $config_->{no_download_mode}) {
    croak "Conflicting options: --first-only and --no-download. Please read --help\n";
  }
  if ( $config_->{remove_old_files}) {
    croak "Conflicting options: --copy and --rmold. You can't use both at the same time.\n";
  }
  
  # Initialize
  initialize('full');
  
  # Make sure the all download directory exists
  croak("The " . $config_->{data_dir} . " directory did not exist!\nAre you sure you have downloaded some podcasts?\n") if(not -d $config_->{data_dir});
    
  my @files_to_delete;
  
  foreach (keys(%{$feeds_})) {
    my $feed = $feeds_->{$_};
    
    foreach my $dl (keys(%{$feed->{old}})){
      if($feed->{old}->{$dl}->{delete}) {
        if(not $config_->{ignore_patter} or not $feed->{old}->{$dl}->{name} =~ /$config_->{ignore_patter}/) {
          push( @files_to_delete, $feed->{old}->{$dl}->{name});
        }
      }
    }
  }
  
  while( scalar( @files_to_delete)) {
    my $file = pop(@files_to_delete);
   
    # If we're in dry run mode then we don't want to actually do anything.
    if ($config_->{dry_run_mode}) {
      printf( "Would delete %s.\n", $file);
    }
    else {
      printf( "Deleting %s...", $file);
      if(unlink($config_->{data_dir} . "/" . $file)) {
        print "done.\n";
      }
      else {
        print "failed: $!\n";
      }
    }
  }
  # Stop here if we're in dry run mode
  if( not $config_->{dry_run_mode}){
    #Todo: Fix cleanupDears
    #cleanupDirs();
  }
  
  return(0);
  
  # Useless usage of some options
  if ($config_->{copy_files_delete} and not $config_->{copy_files_to}) {
    warn "Useless use of --delete without --copy\n";
  }
  if ($config_->{file_number} and 
      not $config_->{copy_files_to} and 
      not $config_->{remove_old_files}) {
    warn "Useless use of --files without --copy or --rmold\n";
  }
  if ($config_->{verbose} and $config_->{no_log} == 2) {
    warn "Useless use of --nolog in verbose mode\n";
  }
  
#  my $CatalogueDirectory = "$config_->{working_dir}/catalogue/";
  # Declare variables
  my (@FileList, %Files, @RemoveTheseFiles);
  my $NumberOfFiles = howManyFiles();
  

  # Figure out which files are the oldest podcasts
  @FileList = reverse sortedFileList($config_->{data_dir});
  my $PodcastCount = @FileList;
  # We don't allow the user to delete the last remaining podcast.
  croak("Only one podcast has been downloaded. If you really want to delete it you must do so manually.\n") if ($PodcastCount == 1);

  # Make sure we don't delete more than the total amount of podcasts minus one
  croak( "$NumberOfFiles is higher than the total amount of podcasts ($PodcastCount).\n" . 
        "If you really want to clean up anyway, use --files to specify how many should be deleted\n") if( $NumberOfFiles >= $PodcastCount);
 
  # Delete the files
  my $DeletedFiles = 0;
  while ($NumberOfFiles > $DeletedFiles) {
    my $TargetBase = basename($FileList[0]);
    if(not $config_->{ignore_patter} or not $TargetBase =~ /$config_->{ignore_patter}/) {
      # If we're in dry run mode then we don't want to actually do anything.
      if ($config_->{dry_run_mode}) {
        print "Would delete $TargetBase\n";
      }
      else {
        print "Deleting $TargetBase...";
        if(unlink($FileList[0])) {
          print "done\n";
        }
        else {
          print "failed: $!\n";
        }
      }
      $DeletedFiles++;
    }
    shift @FileList;
    last if not $FileList[0];
  }
  # Stop here if we're in dry run mode
  exit if $config_->{dry_run_mode};
  cleanupDirs();
  exit( 0 );
}

# Purpose: Copy files to $config_->{copy_files_to}
# Usage: mainCopyFiles();
# ignore_patter, copy_files_delete, dry_run_mode

sub mainCopyFiles
{
  my $copy_files_to = shift;
  
  # Conflicting commandline arguments
  if ($config_->{first_only} and $config_->{no_download_mode}) {
    croak "Conflicting options: --first-only and --no-download. Please read --help\n";
  }
  if ($config_->{remove_old_files}) {
    croak "Conflicting options: --copy and --rmold. You can't use both at the same time.\n";
  }
  
  # Useless usage of some options
  if ($config_->{copy_files_delete} and not $copy_files_to) {
    warn "Useless use of --delete without --copy\n";
  }
  if ($config_->{file_number} and not $copy_files_to and not $config_->{remove_old_files}) {
    warn "Useless use of --files without --copy or --rmold\n";
  }
  if ($config_->{verbose} and $config_->{no_log} == 2) {
    warn "Useless use of --nolog in verbose mode\n";
  }

  # if --ignore-pattern was not supplied but $config_->{ignore_patter} is set in the
  # config file.
  if (not $config_->{ignore_patter}) {
    if ($config_->{ignore_patter_glob}) {
      eval {
        qr/$config_->{ignore_patter_glob}/
      } or croak "The regexp IgnorePattern in the configuration file is invalid ($@)\n";
      $config_->{ignore_patter} = $config_->{ignore_patter_glob};
    }
  }

  # Initialize
  initialize('full');
  
  # Declare variables
  my (@FileList, %Files, @CopyTheseFiles, %DontDeleteThese);
  my $NumberOfFiles = howManyFiles();
  # Do a few directory checks before moving on.
  if(not -e './catalogue/All')
  {
    croak "The ./catalogue/All/ directory did not exist!\nAre you sure you have downloaded some podcasts?\n";
  }
  elsif(not -e $copy_files_to)
  {
    croak "$copy_files_to does not exist!\n";
  }
  elsif(not -d $copy_files_to)
  {
    croak "$copy_files_to is not a directory!\n";
  }
  elsif(not -w $copy_files_to)
  {
    croak "I can't write to the directory $copy_files_to!\n";
  }

  # Figure out which files are the latest podcasts
  # A sorted array of files
  @FileList = sortedFileList( $config_->{working_dir} . "/catalogue/All/" );
  # Create an array of the files we should copy
  my $CopiedFiles = 0;
  while (defined($FileList[$CopiedFiles]) and $CopiedFiles < $NumberOfFiles)
  {
    my $TargetBase = basename($FileList[$CopiedFiles]);
    # Check if we want to skip files matching a specific regexp
    if($config_->{ignore_patter} and not $TargetBase =~ /$config_->{ignore_patter}/)
    {
      push(@CopyTheseFiles, $FileList[$CopiedFiles]);
      $DontDeleteThese{$TargetBase} = 1;
      $CopiedFiles++;
    }
    else
    {
      shift(@FileList);
    }
  }
  # Delete routine (delete files unless we would have copied it)
  if ($config_->{copy_files_delete})
  {
    # Time to delete files in the target directory

    # Babysitting the user :)
    if($copy_files_to =~ m#^($ENV{HOME}(/|/Documents/?.*)|/(usr|var|dev|etc|lib|sbin|sys|boot|proc|opt)/?.*)$#)
    {
      croak "Not allowed to delete in the directory \"$copy_files_to\"\n";
    }
    # Delete the files
    while ($_ = glob("$copy_files_to/*"))
    {
      my $TargetBase = basename($_);
      # This one is merely cosmetic
      $_ =~ s#//#/#g;
      # If it is in the $DontDeleteThis hash or is a directory we skip it.
      if ($DontDeleteThese{$TargetBase} or -d $_)
      {
        next;
      }
      # If we're in dry run mode we don't want to actually do anything
      if ($config_->{dry_run_mode})
      {
        print "Would delete $_\n";
      }
      else
      {
        print "Deleting $_\n";
        unlink($_) or warn "Deleting of $_ failed: $!\n";
      }
    }
  }
  # Copy the files
  foreach (@CopyTheseFiles)
  {
    my $TargetBase = basename($_);
    if (-e $copy_files_to.'/'.$TargetBase)
    {
      printVerbose("Skipping pre-existing \"$TargetBase\"\n");
      next;
    }
    # If we're in dry run mode we don't want to actually do anything
    if ($config_->{dry_run_mode})
    {
      print "Would copy $TargetBase\n";
    }
    else
    {
      print "Copying $TargetBase...\n";
      copy("$_", "$copy_files_to") or croak "Copying failed: $!\n";
    }
  }
  # And finally, attempt to run sync once.
  if (inPath 'sync' and not $config_->{dry_run_mode})
  {
    printVerbose('Synchronizing disks...');
    system('sync');
  }
  printVerbose("All done\n");
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Configuration file functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: Write a configuration file
# Usage: writeConfigFile(/FILE, \%ConfigHash, \%ExplanationHash);
sub writeConfigFile
{
  my ($File, $Config, $Explanations) = @_;

  # Open the config for writing
  open(my $CONFIG, '>', "$File") or do {
    # If we can't then we error out, no need for failsafe stuff - it's just the config file
    warn("Unable to save the configuration file $File: $!");
    return(0);
  };
  if(defined($Explanations->{HEADER})) {
    print $CONFIG "# $Explanations->{HEADER}\n";
  }
  foreach(sort(keys(%{$Config}))) {
    if(defined($Explanations->{$_})) {
      print $CONFIG "\n# $Explanations->{$_}";
    }
    print $CONFIG "\n$_=$Config->{$_}\n";
  }
  close($CONFIG);
}

# Purpose: Load a configuration file
# Usage: loadConfigFile(/FILE, \%ConfigHash, \%OptionRegexHash, OnlyValidOptions?);
#  OptionRegeXhash can be available for only a select few of the config options
#  or skipped completely (by replacing it by undef).
#  If OnlyValidOptions is true it will cause loadConfigFile to skip options not in
#  the OptionRegexHash.
sub loadConfigFile 
{
  return 0 if $config_->{config_loded};
  
  $config_->{config_loded} = 1;
  
  my ($File, $ConfigHash, $OptionRegex, $OnlyValidOptions) = @_;

  my %valid_options = (
     DefaultVerbosity => 'default_verbosity', 
     AutoCleanup      => 'auto_cleanup', 
     PodcastFilter    => 'podcast_filter', 
     WorkingDir       => 'working_dir');

  open(my $CONFIG, '<', "$File") or do {
    warn(sprintf('Unable to read the configuration settings from %s: %s', $File, $!));
    return(0);
  };
  
  my @cfg = <$CONFIG>;
  close($CONFIG);
  
  foreach (@cfg) {
    next if m/^\s*(#.*)?$/;
    next if not m/=/;
    chomp;
    $_ =~ s/\s//g;
    my ($Option, $Value) = split( '=', $_);
    if($OnlyValidOptions) {
      unless(defined($OptionRegex->{$Option})) {
        warn("Unknown configuration option \"$Option\" (=$Value) in $File: Ignored.");
        next;
      }
    }
    unless(defined($Value)) {
      warn("Empty value for option $Option in $File");
    }
    if(defined($OptionRegex) and defined($OptionRegex->{$Option})) {
      my $MustMatch = $OptionRegex->{$Option};
      unless ($Value =~ /$MustMatch/) {
        warn("Invalid setting of $Option (=$Value) in the config file: Must match $OptionRegex->{Option}.");
        next;
      }
    }
    if( defined($valid_options{$Option}) ) {
      $config_->{$valid_options{$Option}} = $Value; 
    }
  }
  return 0;
}

# Purpose: Load the global config file from $config_->{user_config_dir}
# Usage: initGlobalConfig();
sub initGlobalConfig
{
  # Create the directory if it isn't already there
  if(not -e $config_->{user_config_dir})
  {
    initConfigDir();
  }
  my %OptionRegexHash = (
    WorkingDir => '.',
    DefaultVerbosity => '0|1',
    PodcastFilter => '0|1',
  );

  loadConfigFile($config_->{user_config_dir}.'/goldenpod.conf', $config_, \%OptionRegexHash);
  return(1);
}

# Purpose: Write the configuration file
# Usage: WriteConfig();
sub writeGPConfig
{
  # Verify the options first
  if(not defined $config_->{working_dir} or not length($config_->{working_dir}))
  {
    $config_->{working_dir} = "$ENV{HOME}/Podcasts";
  }
  if(not defined($config_->{default_verbosity}) or not length($config_->{default_verbosity}))
  {
    $config_->{default_verbosity} = 1;
  }
  if(not defined($config_->{podcast_filter}) or not length($config_->{podcast_filter}))
  {
    $config_->{podcast_filter} = 0;
  }
  my %Explanations = (
    WorkingDir => "The directory the podcasts will be downloaded to.\n",
    default_verbosity => "How verbose GoldenPod should be by default (commandline arguments overrides this)\n# 1 means be verbose (default), 0 means be silent",
    IgnorePattern => "A regular expression pattern that GoldenPod should ignore when downloading or copying\n# podcasts. It will be applied to the filename. See the manpage for more information about it.\n# --ignore-pattern overrides this setting, and --rmold only obeys --ignore-pattern, not this configuration setting.",
    PodcastFilter => "If GoldenPod should ignore non-audio files in the feeds\n# 0 means don't ignore (default), 1 means ignore.",
    HEADER => "GoldenPod configuration file\n# DO NOT put feed URLs in this file. Those go in podcasts.conf",
  );
  # Write the actual file
  writeConfigFile($config_->{user_config_dir}.'/goldenpod.conf', $config_, \%Explanations);
}

# Purpose: Creates ~/.goldenpod or another $config_->{user_config_dir}
# Usage: initConfigDir();
sub initConfigDir
{
  if(not -e $config_->{user_config_dir})
  {
    mkpath($config_->{user_config_dir}) or croak "Unable to create the directory $config_->{user_config_dir}: $!";
  }
  # If /etc/goldenpod-podcasts.conf exists the copy that to ~/.goldenpod/podcasts.conf
  if ( -e '/etc/goldenpod-podcasts.conf' )
  {
    warn "Copying /etc/goldenpod-podcasts.conf to $config_->{user_config_dir}/podcasts.conf";
    copy('/etc/goldenpod-podcasts.conf', "$config_->{user_config_dir}/podcasts.conf") or warn "Copying of /etc/goldenpod-podcasts.conf failed: $!";
  }
  # If we don't have ~/.goldenpod/podcasts.conf (no /etc/goldenpod.conf or failure copying it)
  # then write an empty one.
  if(not -e "$config_->{user_config_dir}/podcasts.conf" )
  {
    open(my $PODCASTS_CONF, '>',"$config_->{user_config_dir}/podcasts.conf");
    print $PODCASTS_CONF "# Put your podcast feed URLs in this file seperated by newlines\n# All lines starting with # are ignored";
    close($PODCASTS_CONF);
  }
  writeGPConfig();
}

# Purpose: Make us verbose
# Usage: argVerbose();
sub argVerbose
{
  $config_->{verbose} = 1;
  if(not $config_->{no_log})
  {
    $config_->{no_log} = 1;
  }
}

# Purpose: Parse the podcasts.conf file
# Usage: my $confArray = loadFeeds();
sub loadFeeds
{
  return 0 if $config_->{podcasts_loded};
  
  if(not -e $config_->{podcast_list}) {
    croak "The configuration file \"$config_->{podcast_list}\" does not exist!\nPlease read the manpage included to get instructions on how to set one up.\n";
  }
  
  $config_->{podcasts_loded} = 1;
  
  my $currentComments = [];

  $podcasts_->{feeds} = {};
  $podcasts_->{num} = 0;
  
  # Open the configuration file for reading
  open(my $CONFIG, '<', $config_->{podcast_list});
  my @cfg = <$CONFIG>;
  close($CONFIG);
  
  # Read the configuration file and fetch feeds
  while(my $podcast = shift(@cfg))
  {
    chomp $podcast;
    my $match;

    if ($podcast =~ /^\s*#.*/) # Line is comment
    {
      push(@{$currentComments},$podcast);
      next;
    }
    if($podcast =~ /^\s*$/) # Line is empty
    {
      if( not exists($podcasts_->{global_comment})){
        $podcasts_->{global_comment} = $currentComments;
      }
      $currentComments = [];
      next;
    }
    
    my $feed = {comments => $currentComments, match => '', old => {}};
    foreach (split(';', $podcast)){
      my ($key, $value) = split('=', $_);
      $feed->{$key} = $value;
    }
    
    croak "$podcast is no valid configuration" if( not exists($feed->{url}));
    
    $feed->{hash} = md5_hex($feed->{url});
    
#    $podcasts_->{feeds}->{$feed->{url}} = $feed;
#    $podcasts_->{num} += 1;
    
#    $feeds_->{$feed->{url}} = $feed;
    $feeds_->{$feed->{hash}} = $feed;
  }
  return 0;
} # loadFeeds

# Purpose: Write the podcasts.conf file
# Usage: writeFeeds($confArray);
sub writeFeeds
{
#  my $list = shift;
#  return if not $list;
  
  my @cfg;
  
  push( @cfg, '# GoldenPod podcast list. Last written: '.scalar(localtime));
  push( @cfg, "# Put your podcast feed URLs in this file seperated by newlines");
  push( @cfg, "# All lines starting with # are ignored");
  push( @cfg, "");
  
  foreach my $f ( keys(%{$podcasts_->{feeds}})) {
    foreach my $c ( @{$podcasts_->{feeds}->{$f}->{comments}}){
      push( @cfg, $c);
    }
#    url=http://static.orf.at/podcast/fm4/fm4_ombudsmann.xml;name=FM4 Ombutsmann;Age=14
    my $str;
    if( $podcasts_->{feeds}->{$f}->{Age} and $podcasts_->{feeds}->{$f}->{match}){
      $str = sprintf("url=%s;name=%s;Age=%s;match=%s", 
        $podcasts_->{feeds}->{$f}->{url},
        $podcasts_->{feeds}->{$f}->{name},
        $podcasts_->{feeds}->{$f}->{Age},
        $podcasts_->{feeds}->{$f}->{match});
    }
    elsif( $podcasts_->{feeds}->{$f}->{Age}) {
      $str = sprintf("url=%s;name=%s;Age=%s",
        $podcasts_->{feeds}->{$f}->{url},
        $podcasts_->{feeds}->{$f}->{name},
        $podcasts_->{feeds}->{$f}->{Age});
    }
    elsif( $podcasts_->{feeds}->{$f}->{match}) {
      $str = sprintf("url=%s;name=%s;Age=0;match=%s",
        $podcasts_->{feeds}->{$f}->{url},
        $podcasts_->{feeds}->{$f}->{name},
        $podcasts_->{feeds}->{$f}->{match});
    }
    else{
      $str = sprintf("url=%s;name=%s;Age=0",
        $podcasts_->{feeds}->{$f}->{url},
        $podcasts_->{feeds}->{$f}->{name});     
    }
    push( @cfg, $str);
    push( @cfg, "");
  }
  
  if( $config_->{dry_run_mode}) {
    print(join( "\n", @cfg));
    return 0;
  }
  
  open(my $CONFIG, '>', $config_->{podcast_list}) or 
    croak( "Cannot open " . $config_->{podcast_list} . " for write\n");
  
  print $CONFIG join( "\n", @cfg);
    
  close($CONFIG);
  
  return 0;
}

# Purpose: Handle --list to list feeds
# Usage: \&listFeedsExit in GetOptions
sub listFeedsExit
{
  print "\n";
  initialize('util');
  my @normalFeeds;
  my @fuzzyFeeds;
#TODO: Change to PODCAST_CONFIG  !?
  my $feeds = loadFeeds();
  if( $podcasts_->{num} < 1){
    croak("You have no feeds added to GoldenPod\n");
  }
#$podcasts_->{feeds}->{$feed->{url}}
  foreach my $feed (keys(%{$podcasts_->{feeds}}))
  {
    if ($feed->{match})
    {
      push(@fuzzyFeeds,$feed);
    }
    else
    {
      push(@normalFeeds,$feed);
    }
  }

  if (@normalFeeds)
  {
    print "You have the following standard podcast feeds:\n";
    foreach my $feed (@normalFeeds)
    {
      print $feed->{podcast}."\n";
    }
  }
  if (@fuzzyFeeds)
  {
    print "\n" if (@normalFeeds);
    print "You have the following podcasts using the fuzzy parser:\n";
    my $format = '%-20s %s'."\n";
    printf($format,'Regex:','URL:');
    foreach my $feed (@fuzzyFeeds)
    {
      printf($format,'/'.$feed->{match}.'/',$feed->{podcast});
    }
  }
  exit(0);
}

# Purpose: Handle --addfeed to add a feed
# Usage: \&addFeedExit in GetOptions
sub addFeedExit
{
  my $url = shift;
  
  initialize('util');
  
  my $pattern;
  if (@ARGV)
  {
    $pattern = shift(@ARGV);
    $pattern =~ s{^\s*/(.*)/\s*}{$1};
    my $tmp = '';
    if(not eval{$tmp =~ /$pattern/}) {
      croak("The pattern \"$pattern\" does not validate: $@\n");
    }
  }
  if(not $url =~ m{^(http|ftp)})
  {
    print "Warning: This does not look like a feed URL, adding anyway.\n";
    print "If you did not intend to add it, use this command to remove it:\n";
    print "  goldenpod --remove \"$url\"\n\n";
  }

#TODO: Change to PODCAST_CONFIG  
  loadFeeds();
  if( defined( $podcasts_->{feeds}->{$url})){
    
  }
  
  my $feed = fetchURL($url);
  my $xml = XML::Simple->new();
  my $data = $xml->XMLin($feed);
  
  croak( $url." does not seam to be a podcast.\n") if(not defined( $data->{channel}->{item}));
  
  if( $config_->{podcast_name}){
    $podcasts_->{feeds}->{$url}->{name} = $config_->{podcast_name};
  }
  elsif(defined( $data->{channel}->{title})){
    $podcasts_->{feeds}->{$url}->{name} = $data->{channel}->{title};
  }
  else{
    $podcasts_->{feeds}->{$url}->{name} = '';
  }
  
  $podcasts_->{feeds}->{$url} = {
    Age       => $config_->{max_age},
    comments  => ['# Added: '.scalar(localtime)],
    match     => $config_->{podcast_filter},
    url       => $url
  };
  
#  my $sources = locateSourceInFeedList($feed,$feeds);
#  if ($sources)
#  {
#    foreach my $i (@{$sources})
#    {
#      $feeds->[$i] = undef;
#    }
#  }
#  push(@{$feeds}, {
#      podcast => $feed,
#      match => $pattern,
#      comments => ['# Added: '.scalar(localtime)],
#    });


#todo: update write feeds
  writeFeeds();

  if ($pattern)
  {
    print "Added the fuzzy feed \"$url\" with the regex pattern /$pattern/\n";
  }
  else
  {
    print "Added the feed $url\n";
  }
#  if ($sources)
#  {
#    if(scalar @{$sources} > 1)
#    {
#      print 'and removed '.scalar(@{$sources}).' old duplicates.'."\n";
#    }
#    else
#    {
#      print 'and removed one old duplicate.'."\n";
#    }
#  }
} # writeFeeds

# Purpose: Handle --removefeed to remove a feed
# Usage: \&removeFeedExit in GetOptions
sub removeFeedExit
{
  my $feed = shift;
  
  initialize('util');
#TODO: Change to PODCAST_CONFIG  
  my $feeds = loadFeeds();
  my $sources = locateSourceInFeedList($feed,$feeds);
  my $removed = 0;
  if ($sources)
  {
    foreach my $i (@{$sources})
    {
      $feeds->[$i] = undef;
      $removed = 1;
    }
  }
  writeFeeds($feeds);
  if ($removed)
  {
    print "Removed the feed \"$feed\".\n";
  }
  else
  {
    print "The feed \"$feed\" was not found.\n";
  }
  return;
}

# Purpose: Check if a feed contains instances of the supplied source,
#   if it does returns an array of where in the $feeds array they are.
# Usage: my $array = locateSourceInFeedList($feed, $feeds);
sub locateSourceInFeedList
{
  my $feed = shift;
  my $feeds = shift;
  my $found = 0;
  my @sources;
  for my $n (0..@{$feeds})
  {
    my $thisfeed = $feeds->[$n];
    next if not defined $thisfeed or not defined $thisfeed->{podcast};
    if ($feed eq $thisfeed->{podcast})
    {
      push(@sources,$n);
      $found = 1;
    }
  }
  if(not $found)
  {
    return;
  }
  return \@sources;
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Download and feed functions
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# Purpose: Decode XML entities
# Usage: decodedContent = decodeXMLentities(content);
sub decodeXMLentities
{
  my $thing = shift;
  if(not defined $config_->{has}->{HTMLE})
  {
    if(eval{require HTML::Entities})
    {
      $config_->{has}->{HTMLE} = 1;
    }
    else
    {
      $config_->{has}->{HTMLE} = 0;
    }
  }
  if ($config_->{has}->{HTMLE})
  {
    return HTML::Entities->decode_entities($thing);
  }
  else
  {
    $thing =~ s/\&amp;/\&/g;
  }
  return $thing;
}

# Purpose: Get the title from a string
# Usage: title = parseTitle(FEED_CONTENTS);
sub parseTitle
{
  my $parse_title = shift;
  my $FeedTitle = '';
  chomp($parse_title);
  foreach my $t (split(/</i,$parse_title))
  {
    next if not $t =~ s/title[^>]*>//i;
    $FeedTitle = $t;
    if(length($FeedTitle) > 400)
    {
      my $orig = $FeedTitle;
      $FeedTitle = substr($FeedTitle,0,20);
      warn("WARNING: Unreasonably long feed title ($orig). Using $FeedTitle instead\n");
    }
    # Remove stuff in parens, as that often contains stuff like "mp3 feed"
    $FeedTitle =~ s/\([^\)]*\)//;
    $FeedTitle =~ s/<[^>]+>//g;
    $FeedTitle =~ s/\s+/_/g;
    $FeedTitle =~ s#(/|\#|\(|\)|\&|\%|\"|\|)#_#g;
    $FeedTitle =~ s/[\-\_]+(episodes?|promo|mp3|ogg|feed)*$//gi; # Remove various junk
    $FeedTitle =~ s/[\-\_]?$//; # Remove -_ in the end of the name
    # Remove more than one - or _ in a row
    $FeedTitle =~ s/-+/-/g;
    $FeedTitle =~ s/_+/_/g;
    last;
  }
  return $FeedTitle;
}

# Purpose: Ping a feed and output useful information to the user
# Usage: pingFeedExit(anything, URL, secondTry?)
#
# the first arg is ignored, this for use in getOpts, just leave it undef.
# URL is the url to ping
# secondTry is an int if this is our second attempt at parsing a feed.
sub pingFeedExit
{
  my $url = shift;
  my $secondTry = shift;
  my $Output = fetchURL($url);
  croak "\n" if not $Output;
  my ($URLs, $Title) = parseFeed($Output,'.',$url);
  my ($fURLs, $fTitle) = fuzzyparseFeed($Output,'.',$url);
  my $normalResults = scalar @{$URLs};
  my $fuzzyResults = scalar @{$fURLs};
  my $recommendedParser;
  my $baseZero = basename($0);
  my $feed_list = locateFeed($Output);
  print "\n";
  if(not $normalResults and not $fuzzyResults)
  {
    if ($secondTry)
    {
      print "Found nothing using either parser this time either.\n";
      if ($secondTry > 1)
      {
        print "You may want to manually --ping any of the other feeds\n";
        print "listed above.\n";
      }
      croak "\n";
    }
    if ($feed_list and @{$feed_list})
    {
      my $feed = @{$feed_list} > 1 ? 'feeds' : 'feed';
      print "Found nothing using either parser, however, the page\nlisted the following $feed:\n";
      print $_."\n" foreach(@{$feed_list});
      print "\nTrying the feed $feed_list->[0]:\n";
      pingFeedExit(undef,$feed_list->[0],scalar @{$feed_list});
    }
    else
    {
      print "Found nothing using either parser\n";
    }
    croak "\n";
  }
  print 'Found '.$normalResults.' files using the standard/RSS parser'."\n";
  print 'Found '.$fuzzyResults.' files using the fuzzy parser'."\n";
  (my $quotedUrl = $url) =~ s/'/\\'/g;
  if ($fuzzyResults > $normalResults)
  {
    $recommendedParser = 'fuzzy';
    print 'Recommended parser: Fuzzy parser'."\n";
    print 'Add with: '.$baseZero.' --add \''.$quotedUrl.'\' \'/someRegex/\''."\n";
    print 'To simply match/subscribe to all files, use: '.$baseZero.' --add \''.$quotedUrl.'\' \'/./\''."\n\n";
    print 'To subscribe to certain files, use a regex. You can list all files GoldenPod'."\n";
    print 'finds in the feed by running: '.$baseZero.' --fuzzydump \''.$quotedUrl.'\''."\n";
  }
  elsif($normalResults > $fuzzyResults || $normalResults == $fuzzyResults)
  {
    $recommendedParser = 'rss';
    print 'Recommended parser: standard/RSS parser'."\n";
    print 'Add with: '.$baseZero.' --add \''.$quotedUrl."'\n";
  }
  if ($normalResults > 0 && $fuzzyResults < $normalResults)
  {
    print "\n";
    print 'The reason the fuzzy parser found fewer results might be because the'."\n";
    print 'fuzzy parser limits results to only audio files. The recommended'."\n";
    print 'parser to use in these cases is still the RSS parser, as it will provide'."\n";
    print 'better results most of the time.'."\n";
  }
  elsif($normalResults > 0 && $fuzzyResults > $normalResults)
  {
    print "\n";
    print 'The fuzzzy parser found more results than the standard/RSS parser.'."\n";
    print 'This might be a bug in the standard/RSS parser. If this is an RSS feed'."\n";
    print 'please report this as a bug. Otherwise, use the fuzzy parser.'."\n";
  }
  elsif($fuzzyResults == $normalResults)
  {
    print "\n";
    print 'When both parsers return the same amount of results, the recommended'."\n";
    print 'parser is the standard/RSS parser, as it is more restrictive and'."\n";
    print 'accurate.'."\n";
  }
  if ($recommendedParser eq 'fuzzy' && $feed_list && @{$feed_list} > 0)
  {
    print "\n";
    my $feed = @{$feed_list} > 1 ? 'feeds' : 'feed';
    my $other = @{$feed_list} > 1 ? 'other' : 'another';
    my $oneOf = @{$feed_list} > 1 ? ' one of' : '';
    print "NOTE: This URL referenced $other $feed. You may want to try$oneOf the $feed\n";
    $feed = ucfirst($feed);
    print "listed below instead of using the fuzzy parser directly on this URL.\n$feed: ";
    print "\n" if $feed eq 'Feeds';
    foreach my $f(@{$feed_list})
    {
      print $f."\n";
    }
  }
  exit(0);
}

# Purpose: Parse a HTML page, looking for <link rel="alternate"> entries
#   for feeds
# Usage: my $feedURLs = locateFeed(FEED_CONTENTS);
sub locateFeed
{
  my $contents = shift;
  my @URLs;

  foreach my $line (split(/(<|>|\n)/,$contents))
  {
    next if not $line =~ /rel="alternate"/i;
    my $type = $line;
    $type =~ s/.*type=["']([^"']+)["'].*/$1/i;
    next if not $type;
    if(not $type =~ /(rss|atom)\+xml/i and not $line =~ /rss/i)
    {
      next;
    }
    $line =~ s/.*href=["'](\S+)["'].*/$1/gi;
    next if not $line;
    push(@URLs,$line);
  }
  return \@URLs;
}

# Purpose: Parse a feed
# Usage: my($URLs,$Title) = parseFeed(FEED_CONTENTS);
#  $Title can be undef. $URLs is an arrayref, can be empty.
sub parseFeed
{
  my $FeedContents = shift;

  if(not $FeedContents)
  {
    return([],undef);
  }

  my $match = shift;
  my $fullURL = shift;
  my $URL = $fullURL;
  
  my $xml = XML::Simple->new();
  
  my $data = $xml->XMLin($FeedContents);

  my $title;
  my @URLs;
  # This is used to avoid dupes in @URLs
  my %URLList;
  # We don't care about newlines
  $FeedContents =~ s/(\r\n|\r|\n)/ /g;

  # Try to find the title of the podcast
  my $FeedTitle = parseTitle($FeedContents);

  # Do the real parsing and add to @urls
  # We want to extract all url='s
  foreach my $CurrUrl (split(/(<|>)/,$FeedContents))
  {
    next if not $CurrUrl =~ /(url=["'])/i;
    my $quote = $CurrUrl;
    $quote =~ s/.*url=(["']).*/$1/i;
    $CurrUrl =~ s/.*url=$quote([^$quote]+)$quote.*/$1/i;
    $CurrUrl = decodeXMLentities($CurrUrl);
    # Filter away non-audio feeds if the user wants it.
    if ($config_->{podcast_filter})
    {
      next if not basename($CurrUrl) =~ /.*\.$config_->{audio_regex}.*/i;
    }
    # Filter away stuff in $config_->{ignore_patter}
    if ($config_->{ignore_patter})
    {
      next if basename($CurrUrl) =~ /$config_->{ignore_patter}/;
    }
    # Add the title string to the PodNames array (if we have found the title)
    $podcasts_->{names}->{$CurrUrl} = $FeedTitle;
    if(not $URLList{$CurrUrl})
    {
      push(@URLs, $CurrUrl);
      $URLList{$CurrUrl} = 1;
    }
  }
  # The current @URLs has the oldest first. Reverse it before returning it.
  @URLs = reverse(@URLs);
  return(\@URLs, $FeedTitle);
}


 
# Purpose: Fuzzy parse a feed. Gets audio URLs out of a non-RSS (ie. XML, HTML,
#   or any text format really) address
# Usage: my($URLs,$Title) = fuzzyparseFeed(FEED_CONTENTS,regexToMatch,sourceURL);
#INFO: under construction
sub fuzzyparseFeed
{
  my $FeedContents = shift;

  if(not $FeedContents)
  {
    return([],undef);
  }

  my $match = shift;
  my $fullURL = shift;
  my $URL = $fullURL;
  
  my $xml = XML::Simple->new();
  
  my $data = $xml->XMLin($FeedContents);
  
  if(not $URL =~ s{^(\w+://[^/]+).*}{$1} or not $URL =~ s{^([^/]+).*}{$1})
  {
    $URL = undef;
  }
  my $title;
  my @URLs;
  # This is used to avoid dupes in @URLs
  my %URLList;

  # Try to find the title of the podcast
  my $FeedTitle = parseTitle($FeedContents);

  # Do some elaborate fuzzy parsing of the content
  foreach my $e (split(/(\s+|<|>|\)|\(|"|')/,$FeedContents))
  {
    # Get URLs
    if (
      # This one extracts URLs not containing whitespace and \"'
      not $e =~ s#.*(https?[^\\"'\s]+)["']?.*#$1#gi and
      # The same as above, but allows "'
      not $e =~ s#.*(https?\S*)["']?.*#$1#gi and
      # /something/... - ie. without domain and http://
      not $e =~ s#.*["'](/\S+\.\S+)["'].*#$1#
    )
    {
      next;
    }
    # Only audio URLs
    next if not $e =~ /.*[^(www)]\.$config_->{audio_regex}.*/i;
    # Only stuff matching
    next if not $e =~ /$match/;
    chomp($e);

    # If it's a relative URL, do some additional processing
    if ($e =~ m{^/} or not $e =~ m{/})
    {
      # No URL? Then we can't handle it
      if(not $URL)
      {
        next;
      }
      # If fulLURL ends with / use that
      if ($fullURL =~ m{/$})
      {
        $e = $fullURL.$e;
      }
      # If there's .. then try fullURL
      elsif($e =~ m{^/?\.\.} )
      {
        my $u = $fullURL;
        $u =~ s{[^/]+$};
        $e = $u.$e;
      }
      else
      {
        # fall back to URL/e
        $e = $URL.'/'.$e;
      }
    }
    $e = decodeXMLentities($e);
    # Done, add it to the list
    if(not $URLList{$e})
    {
      push(@URLs,$e);
      $URLList{$e} = 1;
    }
  }
  return(\@URLs,$FeedTitle);
}

# Purpose: Download podcasts contained in the supplied array reference
# Usage: downlodPodcasts(ARRAYREF);
sub downlodPodcasts
{
  my $feed = shift;
#  my $DownloadThese = shift;
  my $noTotal = shift;
#TODO: Add first only option

#  my $NeedToDownload = scalar(@{$DownloadThese});
  my @DownloadedFiles;
  
#  $config_->{date} = strftime "%F", localtime;
  # Output the amount of files we need to download
  printVerbose("\n"); # if ($NeedToDownload);
  if(not $noTotal) {
    my $plural = (scalar($feed->{new}) > 1) ? 's' : '';
    printf( "Found a total of %s podcast%s to download.\n\n", 
      scalar($feed->{new}), $plural);
  }

  my $hash = $feed->{hash};
  
  # Download the podcasts
  foreach my $podcast (@{$feed->{new}}) {
    my $url  = $podcast->{url};
    if (not $config_->{dry_run_mode}) {
      print "Downloading $url\n" if not $config_->{verbose};
      # Curl returns nonzero on failure

#todo: change download destination
      my $DownloadStatus = downloadToFile( $config_->{data_dir}, $podcast->{name}, $url);
      if($DownloadStatus) {
        next;
      }
      else {
        push(@DownloadedFiles, $url);
      }
	  # Open the logfile for writing
	    open(my $LOGFILE, '>>',$config_->{podcast_log});
      printf( $LOGFILE "%s:%s:%s:%s\n", $config_->{date}, $hash, $podcast->{hash}, $podcast->{name} );
      close($LOGFILE);
      # If we're in --first-only we add it to $AlreadyDownloaded{$url} so that NoDownload doesn't add it
      if ($config_->{first_only} and $DownloadStatus) {
        $podcasts_->{downloaded}->{$url} = 1;
      }
    }
    else {
      print "Would download $url\n";
    }
  }
  return(\@DownloadedFiles);
}

# Purpose: Create the catalogue entries for the current date
# Usage: createCatalogue();
sub createCatalogue {
  my $Downloaded = shift;
  # Filename filter
  # Remove junk after .EXTension and convert %20 to _
  foreach (glob($config_->{working_dir}.'/'.$config_->{date}.'/*')) {
    my $OldName = basename($_);
    my $NewName = basename($_);
    $NewName =~ s/\?.*//g;
    $NewName =~ s/(%20|\s+)/_/g;
    if(not $NewName eq $OldName) {
      rename($OldName, $NewName);
    }
  }
  # Make the ./latest symlink point to $config_->{date}
  if (-l $config_->{working_dir}.'/latest')  {
    unlink $config_->{working_dir}.'/latest';
  }
  symlink( $config_->{working_dir}.'/'.$config_->{date}, $config_->{working_dir}.'/latest');

  # Create our catalogue directory (podcasts sorted in named directories)
  mkpath($config_->{working_dir}.'/catalogue/All');

  # For every file, make sure it has a catalogue entry.
  foreach my $OrigName (@{$Downloaded})  {
    # We don't want to do anything to playlists
    next if ($OrigName =~ /\.m3u$/);

    my $BaseName = basename($OrigName);
    my ($Existed, $PodBaseName);
    # Get the base name of the podcast
    if ($podcasts_->{names}->{$OrigName})    {
      $PodBaseName = $podcasts_->{names}->{$OrigName};
    }
    else  {
      $PodBaseName = $BaseName;
      # If we couldn't get the podcast name from the feed then try even
      # harder here.
      $PodBaseName =~ s/\d+//g;      # Remove digits
      $PodBaseName =~ s/[\-\_]+(show|promo)*$//gi;  # Remove various junk
      $PodBaseName =~ s/__+/_/g;      # Do some additional cleaning
      $PodBaseName =~ s/[\-\_].$//;      # Remove -_ in the end of the name
      $PodBaseName = "\u$PodBaseName";    # Make the first character be uppercase
      # Give up if we still don't have a name
      if(not $PodBaseName)  {
        $PodBaseName = 'Unknown';
      }
    }
    mkpath($config_->{working_dir}.'/catalogue/'.$PodBaseName);
    # We don't want to do anything if it already exists
    if (not -e $config_->{working_dir}.'/catalogue/'.$PodBaseName.'/'.$BaseName)  {
      # Try to get the extension
      my $NameExtension = $BaseName;
      $NameExtension =~ s/.*(\.\w)/$1/;
      # Symlink the files and write the playlist
      symlink($config_->{working_dir}.'/'.$config_->{date}.'/'.$BaseName,
        $config_->{working_dir}.'/catalogue/'.$PodBaseName.'/'.$BaseName);
      if (-e $config_->{working_dir}.'/catalogue/'.$PodBaseName.'/latest'.$NameExtension)  {
        unlink( $config_->{working_dir}.'/catalogue/'.$PodBaseName.'/latest'.$NameExtension);
      }
      symlink ($config_->{working_dir}.'/'.$config_->{date}.'/'.$BaseName,
        $config_->{working_dir}.'/catalogue/'.$PodBaseName.'/latest'.$NameExtension);
      open(my $PLAYLIST, '>>',$config_->{working_dir}.'/catalogue/'.$PodBaseName.'/'.$PodBaseName.'.m3u');
      print $PLAYLIST "$BaseName\n";
      close($PLAYLIST);
    }
    # Add it to All too if needed
    if (not -e $config_->{working_dir}.'/catalogue/All/'.$BaseName) {
      symlink($config_->{working_dir}.'/'.$config_->{date}.'/'.$BaseName,
        $config_->{working_dir}.'/catalogue/All/'.$BaseName);
    }
  }
}

# Purpose: Do a quick download from a feed
# Usage: mainQuickDownload(anything,URL);
sub mainQuickDownload
{
  my $URL = shift;

  # Initialize
  initialize('full');

  # Download data
  my $Output = fetchURL($URL);

  # Skip if we didn't recieve anything
  croak "\n" if not $Output;

  my $download;

  my ($URLs, $Title) = fuzzyparseFeed($Output,'.',$URL);
  my ($fURLs, $fTitle) = parseFeed($Output);

  if(not @{$URLs} and not @{$fURLs}) {
    croak("No podcasts found in feed\n");
  }
  elsif(@{$URLs} > @{$fURLs} || @{$URLs} == @{$fURLs}) {
    $download = shift(@{$URLs});
  }
  else  {
    $download = shift(@{$fURLs});
  }
  if ($podcasts_->{downloaded}->{$download})  {
    croak( "\nGoldenPod has already downloaded the first podcast in this feed.\n" . 
         "Nothing to do.\n");    
  }
  
  #TODO: Create feed hash for call
  my $files = downlodPodcasts([ $download ], true);
  if(scalar(@{$files})) {
    createCatalogue($files);
  }
  return;
}

# Purpose: Load the list of already downloaded podcasts into %AlreadyDownloaded
# Usge: loadALreadyDownloaded();
sub loadPodcastLog {
  #Return if the logfile is already loaded
  return 0 if $config_->{log_loded};
  
  $config_->{log_loded} = 1;

  loadFeeds();
  
  # Return if no log-file was found
  return 0 if ( not -e $config_->{podcast_log});
  
  my ($y,$m,$d) = split("-", $config_->{date});
  my $today = mktime(0,0,0,$d,$m,$y);

  # Open the logfile containing previously downloaded files
  open(my $LOGFILE, '<', $config_->{podcast_log});
  my @cache = <$LOGFILE>;
  close $LOGFILE;
  
  foreach my $line (@cache){
    chomp $line;
    my( $date, $feed_hash, $podcast_hash, $file_name) = split(":", $line);
    
    # Podcast was previous deleted from config
    next if(not defined( $feeds_->{$feed_hash}));
    
    ($y,$m,$d)     = split("-", $date);
    $date       = mktime(0,0,0,$d,$m,$y);
    my $age        = ($today-$date)/86400;
    
    # Check if file exist
    my $exist = (-e $config_->{data_dir} . "/" . $file_name) ? 1 : 0;
    
    # Delete if older then max age of pc
    my $delete = ( $exist and $age > $feeds_->{$feed_hash}->{Age} and $feeds_->{$feed_hash}->{Age} > 0) ? 1 : 0;
    
    my $podcast = {
      name      => $file_name,
      date      => $date,
      hash      => $podcast_hash,
      new       => 0,
      exist     => $exist,
      delete    => $delete};
       
    $feeds_->{$feed_hash} = {} if( not defined( $feeds_->{$feed_hash}));
    $feeds_->{$feed_hash}->{old} = {} if( not defined( $feeds_->{$feed_hash}->{old}));
    $feeds_->{$feed_hash}->{old}->{$podcast_hash} = $podcast;
  }
  return 0;
}

#TODO: Fix description and change code 
# Purpose: Load the list of already downloaded podcasts into %AlreadyDownloaded
# Usge: loadALreadyDownloaded();
sub createPodcastList {
  # The list of podcasts to download
  my $DownloadQueue;

  # The list of podcasts available
  my @PodcastsAvailable;
  
  my $first_only;
  
  my ($y,$m,$d) = split("-", $config_->{date});
  my $date = mktime(0,0,0,$d,$m,$y);
  
  # Read the configuration file and fetch feeds
  foreach ( keys(%{$feeds_}))  {
    my $feed = $feeds_->{$_};
    my $url = $feed->{url};
    my $match = $feed->{match};
    
    $feed->{name} = '' if( not defined($feed->{name}));
    $feed->{new} = [] if( not defined($feed->{new}));
    
    # Download data
    my $xml_data = fetchURL($url);
    # Skip if we didn't recieve anything
    next if not $xml_data;
    
    # Read in XML to hash
    my $xml = XML::Simple->new();
    my $fdata = $xml->XMLin($xml_data);
 
    if( not $feed->{name}){
      $feed->{name} =  $fdata->{channel}->{title};
    } 
    
    # Push those URLs not already downloaded into @PodcastsAvailable,
    # queueing them for checking and possible downloading later
    my $newP = 0;
    
    my $items = [];
    # Check if the feed has one or more podcasts
    if(ref($fdata->{channel}->{item}) eq 'ARRAY'){
      $items = $fdata->{channel}->{item};
    }
    else {
      push(@{$items}, $fdata->{channel}->{item});
    }
    
    foreach my $t (@{$items}){
      next if( not defined($t->{enclosure})); # Not a valid podcast
      my $pc = $t->{enclosure};
      
      next if( not $pc->{type} =~ m/^audio/); # Not audio 
      next if( not defined($pc->{url})); # Has no URL
            
      my $pc_hash = md5_hex($pc->{url}); # Hash of the url
      
      my @tmp = split("/", $pc->{url});
      my $file_name = pop(@tmp); # The filename of the podcast
 
      if( not exists($feed->{old}->{$pc_hash})) { 
        my $podcast = {
          url     => $pc->{url},
          hash    => $pc_hash,
          date    => $date,
          name    => $file_name,
          new     => 1,
          exist   => 0,
          delete  => 0};
      
        if( not $match or $podcast->{url} =~ /$match/) {
          push(@{$feed->{new}}, $podcast);
          $feed->{old}->{$pc_hash} = $podcast;
          $newP++;
        }
      }
    }

    $first_only = $PodcastsAvailable[0] if $newP;
    if ($newP == 0) {
      if(scalar(@PodcastsAvailable) > 0) {
        printVerbose("No new podcasts found.\n");
      }
      else {
        printVerbose("No podcasts found in feed.\n");
      }
    }
    else {
      my $podcast = $newP == 1 ? 'podcast' : 'podcasts';
      print $newP." new $podcast found\n";
    }
  }
  return 0;
}


# Purpose: Fetch and parse the feeds
# Usage: fetchFeeds();
sub fetchFeeds
{
  # The list of podcasts to download
  my $DownloadQueue;

  # The list of podcasts available
  my @PodcastsAvailable;
  my $first_only;
  
  # Read the configuration file and fetch feeds
  foreach my $url ( keys(%{$feeds_}))
  {
    my $match = $podcasts_->{feeds}->{$url}->{match};

    # Download data
    my $feed = fetchURL($url);
    # Skip if we didn't recieve anything
    next if not $feed;
    
    my $xml = XML::Simple->new();
    my $data = $xml->XMLin($feed);
  
    # Push those URLs not already downloaded into @PodcastsAvailable,
    # queueing them for checking and possible downloading later
    my $newP = 0;
    foreach my $t (@{$data->{channel}->{item}}){
      if($t->{enclosure}->{type} =~ m/^audio/ and 
         not exists($podcasts_->{downloaded}->{$t->{enclosure}->{url}}) and
         (not $match or $t->{enclosure}->{url} =~ /$match/)){
        push(@PodcastsAvailable, [$t->{enclosure}->{url}, $podcasts_->{feeds}->{$url}->{hash}]);
        $newP++;
      }
    }

    $first_only = $PodcastsAvailable[0] if $newP;
    if ($newP == 0) {
      if(scalar(@PodcastsAvailable) > 0) {
        printVerbose("No new podcasts found.\n");
      }
      else {
        printVerbose("No podcasts found in feed.\n");
      }
    }
    else {
      my $podcast = $newP == 1 ? 'podcast' : 'podcasts';
      print $newP." new $podcast found\n";
    }
  }


  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  # Find out if we need to download anything
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if($config_->{first_only}) {
    push(@{$DownloadQueue}, $first_only);
  }
  else {
    $DownloadQueue = \@PodcastsAvailable;
  }

  # If we're in NoDownload or FirstOnly mode, copy the contents of the URLs array into the
  # NoDownload hash.
  if($config_->{no_download_mode} or $config_->{first_only}) {
    %NoDownload = map { $_ => 1 } @PodcastsAvailable;
    if($config_->{no_download_mode}) {
      performNoDownload();
      croak( "\n");
    }
  }
  return($DownloadQueue);
}

# Purpose: Initialize goldenpod
# Usage: initialize($mode)
# $mode is one of
#   full: the default mode
#   util: any secondary utilities, will initialize as usual but skip logging
sub initialize
{
  my $mode = shift;
  
  my $modes = {
    full => 1,
    util => 5,
    uninitialized => 99
  };
  
  state $initialized = $modes->{uninitialized};
  if ( not defined $mode or not defined($modes->{$mode})) {
    croak('initialize() called without a proper $mode');
  }
  if ($initialized and $initialized <= $modes->{$mode} ) {
#    if ($initialized ne $mode) {
#      croak('attempted to re-initialize with a new mode');
#    }
    return(0);
  }

  $initialized = $modes->{$mode};
  # Set the date
  $config_->{date} = strftime "%F", localtime;

  # Load the config
  $config_->{user_config_dir} = $ENV{HOME}.'/.goldenpod';
  
#  initGlobalConfig();
  # Create the directory if it isn't already there
  if(not -d $config_->{user_config_dir}) {
    initConfigDir();
  }
  my %OptionRegexHash = (
    WorkingDir => '.',
    DefaultVerbosity => '0|1',
    PodcastFilter => '0|1',
  );

  loadConfigFile($config_->{user_config_dir}.'/goldenpod.conf', $config_, \%OptionRegexHash);
  # Do base initialization
  $config_->{podcast_log}  = $config_->{user_config_dir}.'/podcasts.log';
  $config_->{podcast_list} = $config_->{user_config_dir}.'/podcasts.conf';  
  
  if(not defined $config_->{working_dir}) {
    $config_->{working_dir} = $ENV{HOME} . '/Podcasts';
    warn('Failed to locate WorkingDir, falling back to '.$config_->{working_dir});
  }

  # Be verbose by default if the user wants to
  if ($config_->{default_verbosity}) {
    argVerbose();
  }
  # Create WorkingDir if it doesn't exist
  mkpath($config_->{working_dir}) if( not -d $config_->{working_dir});
  
  # Require +w on WorkingDir
  if(not -w $config_->{working_dir}) {
    croak("Unable to write to: $config_->{working_dir}: Permission denied\n");
  }
  
  # Get the realpath to WorkingDir in case it is relative
  $config_->{working_dir} = realpath($config_->{working_dir}) or croak("Unable to fix the path of $config_->{working_dir}\n");
  
  $config_->{data_dir} = join('/', $config_->{working_dir}, "audio_files");
  mkpath($config_->{data_dir}) if( not -d $config_->{data_dir});
  if(not -w $config_->{data_dir}) {
    croak("Unable to write to: $config_->{data_dir}: Permission denied\n");
  }
  
  # Prepare logging if needed
  if ($mode eq 'full')
  {
    prepareLogging();
  }
  # Load our alredy downloaded files
  loadPodcastLog();
}

# Purpose: Start the pod-catcher mode
# Usage: podCatcher
sub podCatcher {
#TODO: Check for valide options
  # Conflicting commandline arguments
  if ($config_->{first_only} and $config_->{no_download_mode})
  {
    croak "Conflicting options: --first-only and --no-download. Please read --help\n";
  }
  if ($config_->{copy_files_to} and $config_->{remove_old_files})
  {
    croak "Conflicting options: --copy and --rmold. You can't use both at the same time.\n";
  }  
  
  # Useless usage of some options
  if ($config_->{copy_files_delete} and not $config_->{copy_files_to})
  {
    warn "Useless use of --delete without --copy\n";
  }
  if ($config_->{file_number} and not $config_->{copy_files_to} and not $config_->{remove_old_files})
  {
    warn "Useless use of --files without --copy or --rmold\n";
  }
  if ($config_->{verbose} and $config_->{no_log} == 2)
  {
    warn "Useless use of --nolog in verbose mode\n";
  }
  
  # if --ignore-pattern was not supplied but $config_->{ignore_patter} is set in the
  # config file.
  if (not $config_->{ignore_patter})
  {
    if ($config_->{ignore_patter_glob})
    {
      eval { qr/$config_->{ignore_patter_glob}/ } 
        or croak "The regexp IgnorePattern in the configuration file is invalid ($@)\n";
      $config_->{ignore_patter} = $config_->{ignore_patter_glob};
    }
  }

  # Initialize
  initialize('full');
  
  # If the config is empty, die.
  if (not scalar(keys(%{$feeds_}))) {
    croak "The podcast list in $config_->{podcast_list} is empty, nothing to do.\n";
  }
  
  #Clean-up out-dated downloads
  deleteOldPodcasts(); 
  
  #TODO: Add comment
  createPodcastList();

  # Fetch and parse our feeds.

  # Then download the podcasts
  foreach (keys(%{$feeds_})){
    my $test = $feeds_->{$_}->{name};
    my $Downloaded = downlodPodcasts($feeds_->{$_});
  }
  
  createPlaylists();
  
  my $Downloaded;
  
  if(scalar(@{$Downloaded}))
  {
    createCatalogue($Downloaded);
    # NoDownload if in --first-only
    if($config_->{first_only})
    {
      performNoDownload();
    }
  }
}

sub writeM3U {
  # The name of the playlist to write
  my $playlist = shift;
  # Array with the files to add to the playlist
  my $files = shift;
  
  open( my $FILE, '>', $playlist) or 
    croak( "Cannot open " . $playlist . " for write\n");
  
  print( $FILE  join("\n", @{$files}));
  
  close $FILE;
  
  return 0; 
}

sub createPlaylists {
  my $playlist_dir = $config_->{working_dir} . "/playlists";
  
  if(not -d $playlist_dir) {
    mkdir( $playlist_dir) or croak("Unable to mkdir " . $playlist_dir . ": $!\n");
  }
  
  my $playlists = {
    all => [],
    $config_->{date} => [],
    new => [] };
  

  
  my $tlist = {};
  
  #TODO: create playlists
  foreach (keys(%{$feeds_})) {
    my $feed = $feeds_->{$_};
    foreach (keys(%{$feed->{old}})) {
      my $podcast = $feed->{old}->{$_};
      
      if($podcast->{exist}){
        my $date = $podcast->{date};
        
        $tlist->{$date} = [] if( not exists($tlist->{$date}));
        
        $podcast->{fname} = $feed->{name};
        
        push( @{$tlist->{$date}}, $podcast);
      }
    }
  }
  
  my @dates = sort(keys(%{$tlist}));
  
  my ($y,$m,$d) = split("-", $config_->{date});
  my $today = mktime(0,0,0,$d,$m,$y);  
  
  foreach my $date (@dates){
    foreach my $podcast (@{$tlist->{$date}}){
      my $path = "../audio_files/" . $podcast->{name};
      
      #<podcast>.m3u
      $playlists->{$podcast->{fname}} = [] if(not exists($playlists->{$podcast->{fname}}));
      push(@{$playlists->{$podcast->{fname}}}, $path);
     
      #all.m3u:
      push(@{$playlists->{all}}, $path);
      
      #new.m3u
      push(@{$playlists->{new}}, $path) if( $podcast->{new});
            
      #<date>.m3u:
      push(@{$playlists->{$config_->{date}}}, $path) if($today == $date);
    }
  }
  
  foreach my $key ( keys(%{$playlists})) {
    if( scalar($playlists->{$key})){
      my $name = $playlist_dir . "/" . $key . ".m3u";
      writeM3U( $name, $playlists->{$key});
    }
  }
  
  return 0;
}

sub fuzzyDumpExit {
  my $url = shift;
  my $Output = fetchURL($url);
  exit if not $Output;
  my ($URLs, $Title) = fuzzyparseFeed($Output,'.',$url);
  if (not @{$URLs})
  {
    print "Found no audio files\n";
    exit(0);
  }
  print "Page is '$Title', found the following audio files:\n";
  foreach my $f (@{$URLs})
  {
    print $f."\n";
  }
}

sub rssDumpExit {
  my $url = shift;
  my $Output = fetchURL($url);
  exit if not $Output;
  my ($URLs, $Title) = parseFeed($Output,'.',$url);
  if (not @{$URLs})
  {
    print "Found no files\n";
    exit(0);
  }
  print "Page is '$Title', found the following files:\n";
  foreach my $f (@{$URLs})
  {
    print $f."\n";
  }
}
  
sub printStatsExit {
  initialize('util');
  croak "The \"catalogue/All\" directory did not exist, are you sure you have downloaded anything?\n" if not -d "$config_->{working_dir}/catalogue/All";
  my @PodcastFileList = sortedFileList($config_->{working_dir}.'/catalogue/All');
  print "\nYou have ", scalar @PodcastFileList, " files\n";
  print 'The newest file is: ', basename($PodcastFileList[0]),"\n";
  print 'The oldest file is: ', basename($PodcastFileList[-1]),"\n";
  # Find filesizes
  my $USED_DISKSPACE = 0;
  foreach (glob($config_->{working_dir}.'/catalogue/All/*')) {
    # Skip the file if -s doesn't return anything useful.
    if (-s $_) {
      $USED_DISKSPACE = $USED_DISKSPACE+-s $_;
    }
  }
  $USED_DISKSPACE = $USED_DISKSPACE/1024/1024;
  print 'The files are using ', sprintf ('%.0f', $USED_DISKSPACE), " MB of diskspace\n";
  exit 0
}
  
sub setSilentMode {
  $config_->{verbose} = 0;
  if(not $config_->{no_log} == 2)
  {
    $config_->{no_log} = 0;
  }
}
  
sub ignorePattern {
  shift;
  $config_->{ignore_patter} = shift;
  eval { qr/$config_->{ignore_patter}/ } or croak "The regexp supplied to --ignore-pattern is invalid ($@)\n";
}


# Purpose: The main function
# Usage: main();
sub main {
  # print \n if we're in verbose mode
  printVerbose("\n");
  
  my $n_commands = scalar( keys(%{$config_->{commands}}));
  
  if( $n_commands < 0 or $n_commands > 1 ){
    print( 'The options you have entered are not valid!\n');
    croak 'Run ', basename($0), " --help for help\n";
  }
  
  my @cmd;
  if( not $n_commands ){
    $cmd[0] = 'run';
  }
  else {
    @cmd = keys(%{$config_->{commands}});
  }
  
  my $actions = {
    'run'       => sub{ podCatcher()},
    'copy'      => sub{ mainCopyFiles($config_->{commands}->{copy}) },
    'quick'     => sub{ mainQuickDownload($config_->{commands}->{quick}) },
    'ping'      => sub{ pingFeedExit($config_->{commands}->{ping}) },
    'rssdump'   => sub{ rssDumpExit($config_->{commands}->{rssdump}) },
    'fuzzydump' => sub{ fuzzyDumpExit($config_->{commands}->{fuzzydump}) },
    'remove'    => sub{ removeFeedExit($config_->{commands}->{remove}) },
    'add'       => sub{ addFeedExit($config_->{commands}->{add}) },
    
    'rmold'     => sub{ deleteOldPodcasts() },
    'dumpinfo'  => sub{ dumpInfo(1) },
    'sats'      => sub{ printStatsExit },
    'list'      => sub{ listFeedsExit },
    'clean'     => sub{ cleanupDirs() },
    'help_all'  => sub{ help(1) },
    'help'      => sub{ help() },
    'version'   => sub{ printVersion() }};
    
  if(exists($actions->{$cmd[0]})){
    $actions->{$cmd[0]}->();
  }
  else {
    croak( "no valid command entered\n");
  }
 
  return;
}


main();

__END__
=encoding utf8

=head1 NAME

goldenpod - a command-line podcast client written in perl

=head1 SYNOPSIS

B<goldenpod> [I<OPTIONS>]

=head1 DESCRIPTION

B<GoldenPod> is a command-line podcast client (or podcast aggregator, or
podcatcher, feel free to pick whichever name you want) written in perl.

It reads from configuration files in ~/.goldenpod, and saves podcasts to
the directory defined there (by default ~/Podcasts/).

=head1 BASIC USE

=head2 Adding feeds

Adding podcast feeds to goldenpod is simple. Just run:

  goldenpod --add [URL]

And the URL supplied is added to goldenpod, and it will download podcasts
from it the next time it is started.

If the podcast does not have a proper feed, you can try to use the
goldenpod fuzzy parser (see the PARSERS section further down for information
about it). The syntax for that is:

  goldenpod --add [URL] /REGEX/

If you have added a feed using the fuzzy parser and want the standard parser, or
the other way around, simply use the --add parameter again, with the syntax you
want and any existing entries of that URL will be removed first.

If you are uncertain about which parser to choose, run:

  goldenpod --ping [URL]

It will then tell you which parser it recommends for the URL supplied.

=head2 Removing feeds

Removing a feed is just like --add, use:

  goldenpod --remove [URL]

This will remove all feeds (no matter which parser is used) at the URL
supplied.

=head2 Listing feeds

To get a list of all the feeds you have added, along with which parser
they are using, run:

  goldenpod --list

=head2 Downloading podcasts

To download podcasts from the feeds you have added, run goldenpod without
any parameters.

=head1 OPTIONS

=over

=item B<-h, --help>

Display the help screen

=item B<-v, --verbose>

Be verbose.

=item B<-s, --silent>

Be silent.

=item B<-l, --nolog>

Don't create a message logfile when in non-verbose mode. No effect unless in
B<--silent> mode.

=item B<-a, --add, --addfeed I<FEED> (I<REGEX>) >

Add the feed URL supplied to goldenpod's feed list. Optionally you may supply
a second parameter to this option, a regular expression. If this is present
then the feed will be added to use the fuzzy parser (see the PARSERS section)
with that regular expression.

=item B<-r, --remove, --removefeed, --rmfeed I<FEED> >

Removes the feed URL supplied from goldenpod's feed list.

=item B<-t, --list, --listfeeds>

Prints a list of feeds in goldenpod, along with which parser the feeds are
using.

=item B<--ping I<URL>>

This downloads URL and parses it once with each parser, then tells you
which parser it recommends that you to use. It will also attempt feed
autodiscovery if the URL is not an RSS feed.

=item B<--prefer-curl>

Prefer to use curl over LWP if present. GoldenPod uses LWP by default if it is
available, and falls back to curl if LWP is missing. This parameter reverses
this behaviour, using curl if it is available and falling back to LWP if curl
is missing.

=item B<--debuginfo>

Print the configuration and podcast list filenames that GoldenPod would read,
in addition to the logfiles it would use and write to. Additionally it includes
some information about versions of libraries and utilities that gpgpwd uses.

=item B<--stats>

Print some simple statistics: How many files you currently have in your
catalogue, which file is the latest, which file is the oldest and how much
space they are using.

=item B<-u, --dry-run>

Used along with --copy or --rmold. Just display what would be done, don't
actually copy or delete anything.  Implies B<--verbose>

=item B<-w, --no-download>

Mark all podcasts as downloaded.  Useful when you want to subscribe to a
podcast but not download all of the old issues. You can edit the logfile
afterwards and remove those you want to download.  Implies B<--verbose>.

=item B<-f, --first-only>

Download the first file in any feed, and then permanently ignore the others.
If you at any later point want to download older files, you will need to edit
the logfile. Unlike --no-download this does not imply --verbose.

=item B<--quick I<URL>>

Download the first podcast found in URL and then exit. This can be useful
for one-off downloads, or to just check out the latest episode of a podcast
before adding a full subscription. This will try both parsers, the one that
finds the most will be used.

=item B<-c, --copy I</path>>

Copy the last N downloaded files to /path/ and delete the other files in
/path/.  This is very useful for synchronizing the latest podcasts with your
MP3 player.  N is either 4 by default or optionally the value supplied to
--files.

=item B<-d, --delete>

For use with --copy, delete all files in /path/ unless they are one of the
files we are about to copy. It will not allow you to delete files directly in
your home directory (but all subdirectories except Documents) nor any files in
/usr /var /dev /etc /lib /sbin /sys /boot or /proc.

=item B<-n, --files I<N>>

For use with --copy or --rmold. Copy/delete N files instead of 4.

=item B<--fuzzydump I<URL>>

This uses the GoldenPod fuzzy parser to list audio files found in I<URL>.
The fuzzy parser is the one invoked when using the "/REGEX/ URL" syntax
in the podcasts.conf file (ie. to fetch podcasts from a non-RSS source).

You can use this to see if GoldenPod finds anything in the URL, and
to find out what your /REGEX/ for the URL should be.

=item B<--rssdump I<URL>>

This uses the GoldenPod standard/RSS parser to list files found in I<URL>.

You can use this to see if GoldenPod finds anything in the URL. It is mostly
useful to find out if GoldenPod has problems parsing a feed, or if a certain
feed needs the fuzzy parser to work properly.

=item B<-o, --rmold>

Delete N old podcasts, where N is either 4 or optionally the value supplied to
--files. Use this to free up some disk space taken up by old podcasts.  This
will obey --ignore-pattern but not the I<IgnorePattern> configuration option.
It will always leave at least the latest podcast.

=item B<-i, --ignore-pattern>

Ignore files matching the regular expression pattern supplied when downloading
or copying podcasts.

For example: "--ignore-pattern foo" would ignore any podcast containing the
word "foo" in its filename, or for a more advanced example: "--ignore-pattern
'(foo|bar|baz)'" would ignore all podcasts containing either of the words foo,
bar or baz in it's filename. Like everything else the --ignore-pattern
expression is case sensitive.  If you would like to match both Foo and foo you
could do: "--ignore-pattern [f|F]oo".  --ignore-pattern supports standard perl
regular expressions (will be executed within m//).

=item B<--clean>

Clean up the podcasts directory and catalogue. This is useful if you removed
files by hand. It will remove empty directories, remove orphaned symlinks in
the catalogue and rewrite playlists. This is also done after --rmold.

=back

=head1 PARSERS

GoldenPod comes with two different feed parsers.

=head2 STANDARD/RSS PARSER

This is the default parser. It parses any standard RSS feed used by podcasts.
This is the most commonly used parser, and is preferred whenever possible.
This is used unless the fuzzy parser is explicitly requested.

=head2 FUZZY PARSER

This is a much more liberal parser. It searches for URL-like strings in any
document that matches a set regular expression, allowing you to subscribe
to podcasts that does not have their own feed, or to subscribe to
audio-files found on a page regulary for download. This parser
allows you to subscribe to sites that has the links to the audio
files in a HTML, XML or other text-based format.

It has limitations however, it only finds audio files and it requires a regular
expression. It will only download files that match the regular expression that
you supply to it (if you want it to download all audio files it finds, make the
regular expression a single "." (without the quotes) to make it match them all).

To use the fuzzy parser, supply a regular expression as the second parameter
to I<--add> (or, if you edit the podcasts.conf file by hand, use the "/regex/
URL" syntax).

=head1 EXAMPLE podcasts.conf

  # Put your podcast feed URLs in this file seperated by newlines
  # All lines starting with # are ignored

  # Paranormal podcast
  http://paranormalpodcast.libsyn.com/rss

  # Perlcast (perl related podcast)
  http://www.perlcast.com/rss/current.xml

  # LUG Radio
  http://www.lugradio.org/episodes.rss

  # Jawbone radio
  http://feeds.feedburner.com/JawboneRadio

  # SOE podcast. Not using a feed, but extracted by GoldenPod from the
  # HTML, locating any audio file matching the perl regex /96/
  /96/ http://www.station.sony.com/en/podcasts.vm

=head1 HELP/SUPPORT

If you need additional help, please visit the website at
L<http://random.zerodogg.org/goldenpod>

=head1 DEPENDENCIES

Besides from perl it requires either LWP (preferred) or curl.

=head1 INCOMPATIBILITIES

The configuration syntax changed in 0.7, and GoldenPod versions older than that
will fail to read the current configuration file. 0.6 and older also did not
have support for fuzzy parsing (the "/REGEX/ URL" syntax) and will fail to
download those entries.

The directories created by 0.7 and older will always be YYYY-MM-DD, while 0.1
to 0.6 could be YYYY-M-D.

=head1 BUGS AND LIMITATIONS

If you find a bug, please report it at L<http://random.zerodogg.org/goldenpod/bugs>

=head1 AUTHOR

B<GoldenPod> is written by Eskild Hustvedt I<<code aatt zerodogg d0t org>>

=head1 FILES

=over

=item I<~/.goldenpod/podcasts.conf>

The file containing a list of podcast feeds.

=item I<~/.goldenpod/goldenpod.conf>

The configuration file for GoldenPod, which contains the location to save
streams to, your desired verbosity level, the default IgnorePattern and if you
want to use the PodcastFilter.

=item I<~/.goldenpod/podcasts.log>

The logfile containing the URLs for the podcasts already downloaded.

=item I<~/.goldenpod/goldenpod.log>

The logfile written to when in silent mode.

=item I</etc/goldenpod-podcasts.conf>

This is a file in the same syntax as podcasts.conf. It is copied to
~/.goldenpod/podcasts.conf the first time goldenpod is run if it exists. This
file is never read directly, and has no effect whatsoever after the first time
goldenpod is run.

=back

=head1 LICENSE AND COPYRIGHT

Copyright (C) Eskild Hustvedt 2005, 2006, 2007, 2009, 2010

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see L<http://www.gnu.org/licenses/>.
