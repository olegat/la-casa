#!/usr/bin/python
# -*- Mode: python; indent-tabs-mode: nil; python-indent: 2 -*-
#
# @author      Oli Legat <olegat@google.com>
# @copyright   Google Inc, do not redistribute without written permission.
# @brief       Update all cloud gamelet pools in current project.
# @usage       Run with --help


import argparse
import json
import os
import subprocess
import time

opt = None



#------------------------------------------------------------------------------
# General utility
#------------------------------------------------------------------------------
def print_error(msg):
  print 'error: ' + str(msg)
  return False


#------------------------------------------------------------------------------
# ggp command-line handling
#------------------------------------------------------------------------------
def ggp(args):
  global opt
  ggp_args = ['ggp', '-s'] + args

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
    return json.loads(stdout)
  else:
    return None



#------------------------------------------------------------------------------
# Update all cloud pools
#------------------------------------------------------------------------------
def update_pool(version, pool_name, pool_id):
  print 'Updating pool "'+pool_name+'" to version '+version
  ggp(["pool", "update", "--software-version="+version, pool_id])

def run_update(version):
  all_pools = ggp(["pool", "list"])
  for pool in all_pools:
    # Update non-test shape cloud pools
    if (pool["hostType"] == "CLOUD_QUOTA" and
        not pool["instanceShapeSpec"].startswith("test-") ):
      update_pool( version, pool["displayName"], pool["id"] )
  



#------------------------------------------------------------------------------
# Main Entry
#------------------------------------------------------------------------------
if __name__ == '__main__':
  # Parse arguments
  parser = argparse.ArgumentParser(
    description="Update all cloud gamelet pools in current project..",
    formatter_class=lambda prog: argparse.HelpFormatter(prog,max_help_position=50))
  parser.add_argument(
    '--software-version', dest='software_version', type=str,
    help='(required) Software version of instances.')
  opt = parser.parse_args()

  if not opt.software_version:
    print_error('--software-version is required.')
    exit(255)
  else:
    run_update( opt.software_version )
