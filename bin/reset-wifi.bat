#!/bin/perl

$| = 1;
use strict;
use warnings;
use IO::File;
use POSIX qw(strftime);
use File::Spec;

my $fh    = new IO::File ">> $0.log";
my $debug = 0;
my $sleep = 10;

main();

sub println {
  my ($text) = @_;
  $text =~ s/\r//g;
  $text = strftime('%Y-%m-%d %H:%M:%S', localtime) . " $text\n";
  print $fh $text;
  print $text;
}
sub psystem {
  my ($cmd) = @_;
  println $cmd;
  $cmd = "echo $cmd" if ($debug);
  my $result = `$cmd 2>&1`;
  println $result;
}

sub main {
  sleep 20 if !$debug;
  snoretoast("starting");
  check();
}

BEGIN {
  my $counter = 0;
  sub reconnect {
    ++$counter;
    psystem "netsh wlan disconnect";
    snoretoast("Disconnect. ($counter)");
    mysleep(5);
    psystem "netsh wlan connect name=Watzmann";
    snoretoast("Connect. ($counter)");
    $sleep = 10;
  }
}

sub check {
  for (my $t0 = time ; time < $t0 + 2 * 60 ;) {
    println "checking...";
    mysleep();
    my $ping = qx(ping 8.8.8.8 -n 1);
    println $ping;
    if ($ping !~ /Empfangen = 0/) {
      next;
    }

    println "Resetting...";
    reconnect();
  }
}

sub snoretoast {
  my ($msg) = @_;
  my ($volume, $directories, $file) = File::Spec->splitpath($0);
  my $id = "reset-wifi";
  my $exe = File::Spec->catpath($volume, $directories, "snoretoast.exe")
      . " -id $id";
  psystem $exe . " -close";
  psystem $exe . " -t \"reset-wifi\" -m \"$msg\"";
}

sub mysleep {
  my ($seconds) = @_;
  $seconds = $seconds || $sleep;
  println "Sleep $seconds";
  sleep $seconds;
}
