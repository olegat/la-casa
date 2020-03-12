#!/usr/bin/python
# -*- Mode: python; indent-tabs-mode: nil; python-indent: 2 -*-
#
# @author      Oli Legat <olegat@google.com>
# @copyright   Google Inc, do not redistribute without written permission.
# @brief       Testing for "ggp network firewall"
#   see: https://docs.google.com/document/d/1438tZFUCVxdH7bOk7hkpu8VpYVQQQOWzHonVqpCtc_A/edit


import argparse
import os
import subprocess
import random


#------------------------------------------------------------------------------
# ggp command-line handling
#------------------------------------------------------------------------------
# Usage example: json.loads( ggp('-s', 'instance', 'list', '--all') )
def ggp(*args):
  ggp_args = ['ggp'] + list(args)

  # Open ggp.exe subprocess and read output/err
  ggp = subprocess.Popen(
    ggp_args
    ,stdout=subprocess.PIPE
    ,stderr=subprocess.STDOUT
  )
  stdout = ggp.stdout.read()
  ggp.wait()

  # Print errors
  if ggp.returncode:
    # Failure
    if stdout:
      print "ggp process failed :-("
      print "Exit code:  "+ str(ggp.returncode)
      print "Args:       " + " ".join(ggp_args)
      print "Output:"
      print stdout
      exit(ggp.returncode)
    return None

  # Success
  if stdout:
    return stdout
  else:
    return ""


#------------------------------------------------------------------------------
#  Firewall Config Generation
#------------------------------------------------------------------------------
def CIDR_generator_mersennetwister():
  def octet():
    return str(random.randrange(1 ,256))
  return '.'.join([ octet(), octet(), octet(), octet() ]) + '/32'

def generate_CIDR_notations(generator_fn, count):
  result = [None] * count # reserve a list that's big enough
  for i in range(count):
    result[i] = generator_fn()
  return result

def generate_firewall_rules():
  protocols = ['streamer-client','ssh','multiplayer','stream-connect']
  access_level = 'allow-subnets'
  rules = [None] * len(protocols)
  for i in range(len(protocols)):
    rules[i] = protocols[i] + ':' + access_level
  return rules


#------------------------------------------------------------------------------
#  Contruct command line args
#------------------------------------------------------------------------------
def build_flaglist(flag, flag_values):
  return [ flag + '=' + ','.join(flag_values) ]

def build_firewall_rules():
  rules = generate_firewall_rules()
  return build_flaglist( '--rule', rules )

def build_firewall_subnets(n):
  subnets = generate_CIDR_notations( CIDR_generator_mersennetwister, n )
  return build_flaglist( '--subnet', subnets )


#------------------------------------------------------------------------------
# Main Entry
#------------------------------------------------------------------------------
help_description= """Test "ggp network firewall update" by bombarding it with
arguments. To pass additional flags to ggp.exe, use the "--" flag to tell the
script to stop parsing options."""
def main():
  # Parse arguments
  global help_description
  parser = argparse.ArgumentParser(description=help_description,
    formatter_class=lambda prog: argparse.HelpFormatter(prog,max_help_position=50))
  parser.add_argument('--n-subnets', dest='n_subnets', type=int, nargs=1, help="number of random subnets to use")
  parser.add_argument('args', type=str, nargs='*', help="additional arguments to pass to ggp.exe")
  opt = parser.parse_args()

  # Build args for ggp.exe
  rules = build_firewall_rules()
  subnets = []
  if opt.n_subnets:
    subnets = build_firewall_subnets(opt.n_subnets[0])
  args = rules + subnets + opt.args

  # Run ggp.exe
  out = ggp( 'network', 'firewall', 'update', *args )
  print(out)


if __name__ == '__main__':
  main()
