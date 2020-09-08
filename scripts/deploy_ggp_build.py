#!/usr/bin/python
# -*- Mode: python; indent-tabs-mode: nil; python-indent: 2 -*-
#
# @author      Oli Legat <olegat@google.com>
# @copyright   Google Inc, do not redistribute without written permission.
# @brief       Automatically deploy a build onto GGP for Cert.
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
def makedirs(dirs):
  if not os.path.exists(dirs):
    os.makedirs(dirs)

def make_parent_dirs(path):
  makedirs(os.path.dirname(path))

def print_error(msg):
  print 'error: ' + str(msg)
  return False


#------------------------------------------------------------------------------
# ggp command-line handling
#------------------------------------------------------------------------------
def ggp(args):
  global opt
  ggp_args = ['ggp', '-s'] + args
  if opt.dry:
    print ' '.join(ggp_args)
    return None

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
# JSON manipulation
#------------------------------------------------------------------------------
def load_json(content_dir, filename):
  with open(content_dir + '/' + filename) as f:
    return json.load(f)

def dump_json(content_dir, filename, content_json):
  with open(content_dir + '/' + filename, 'w') as f:
    return json.dump(content_json, f, indent=2)

def set_package_id(content_dict, new_package_id):
  content_dict['application_content']['package']['package_id'] = new_package_id



#------------------------------------------------------------------------------
# Step 1: Create package
#------------------------------------------------------------------------------
def create_package(build_dir, package_path):
  return ggp(['package', 'create-image',
              '--content', build_dir,
              '--image', package_path])



#------------------------------------------------------------------------------
# Step 2: Upload package
#------------------------------------------------------------------------------
def upload_project_package(package_path, display_name, game_args):
  return ggp(['package', 'upload',
              '--image', package_path,
              '--display-name', display_name,
              '--'] + game_args)             
def upload_title_package(title_id, package_path, display_name, game_args):
  return ggp(['package', 'upload',
              '--title', title_id,
              '--image', package_path,
              '--display-name', display_name,
              '--'] + game_args)
def upload_package(package_path, display_name, game_args):
  global opt
  if not opt.title:
    return upload_project_package( package_path, display_name, game_args )
  else:
    return upload_title_package( opt.title, package_path, display_name, game_args )



#------------------------------------------------------------------------------
# Step 3: Get Package ID
#------------------------------------------------------------------------------
def get_project_package_list():
  return ggp(['package', 'list'])
def get_title_package_list(title_id):
  return ggp(['package', 'list', '--title', title_id])

def get_package_id():
  global opt
  pkgs = []
  if not opt.title:
    pkgs = get_project_package_list()
  else:
    pkgs = get_title_package_list( opt.title )
  return pkgs[0]['id']



#------------------------------------------------------------------------------
# Step 4: Edit JSON Package ID
#------------------------------------------------------------------------------
def edit_package_id(content_dir, new_package_id):
  content = load_json( content_dir, "content.json" )
  set_package_id( content, new_package_id )
  dump_json( content_dir, "content.json", content )



#------------------------------------------------------------------------------
# Step 5: Create release
#------------------------------------------------------------------------------
def create_release(content_dir, release_path):
  return ggp(['title', 'release', 'create-image',
              '--content', content_dir,
              '--image', release_path])



#------------------------------------------------------------------------------
# Step 6: Wait for package processing
#------------------------------------------------------------------------------
def wait_package_ready(package_id):
  global opt
  args = []
  if opt.title:
    args = ['--title', opt.title]

  done = False
  while not done:
    # The command 'ggp package describe' doesn't have a --title option.
    # Use package list instead
    pkg = ggp(['package', 'list', '--all'] + args + [package_id])[0]
    if pkg['status'] == 'READY':
      done = True
    else:
      time.sleep(1)



#------------------------------------------------------------------------------
# Step 7: Upload release
#------------------------------------------------------------------------------
def upload_title_release(title_id, release_path):
  return ggp(['title', 'release', 'upload',
              '--title', title_id,
              '--path', release_path])
def upload_application_release(app_id, release_path):
  return ggp(['application', 'release', 'upload',
              '--application', app_id,
              '--path', release_path])
def upload_release(release_path):
  global opt
  if not opt.title:
    return upload_application_release( opt.app, release_path )
  else:
    return upload_title_release( opt.title, release_path )



#------------------------------------------------------------------------------
# Run all steps
#------------------------------------------------------------------------------
def print_step(msg):
  global opt
  if not opt.dry:
    print msg

def run_deployment():
  # Create intermediate folder
  global opt
  makedirs(opt.int_dir)

  build      = opt.build_dir
  package    = opt.int_dir + '/package.tar'
  release    = opt.int_dir + '/release.tar'
  package_id = opt.pkg

  # Create / Upload package
  if package_id:
    print_step('Skipping package create/upload')
  else:
    print_step('Creating package:  '+package)
    create_package(  opt.build_dir, package )
    print_step('Uploading package: '+package)
    upload_package(  package, opt.display_name, opt.game_args )

  # Update package ID
  if not opt.dry:
    print_step('Editing '+opt.release_dir+'/content.json')
    package_id = get_package_id()
    edit_package_id( opt.release_dir, package_id )
    print_step('Waiting for package '+package_id+' to be ready')
    wait_package_ready( package_id )

  # Create / Upload release
  print_step('Creating release:  '+release)
  create_release( opt.release_dir, release )
  print_step('Uploading release: '+release)
  upload_release( release )

  return 0


#------------------------------------------------------------------------------
# Argument validation:
#------------------------------------------------------------------------------
def validate_build(package_id, directory, game_args):
  if not package_id:
    if not directory:
      return print_error('required --build-dir missing')
    game_bin = directory + '/' + game_args[0]
    if not os.path.isfile(game_bin):
      return print_error('executable file '+game_bin+' does not exist')
  return True

def validate_display_name(package_id, display_name):
  if not package_id and not display_name:
    return print_error('required --display-name missing')
  return True

def validate_release_dir(release_dir):
  if not release_dir:
    return print_error('required --release-dir missing')
  json_path = release_dir + '/content.json'
  if not os.path.isfile(json_path):
    return print_error('JSON file '+json_path+' does not exist')
  else:
    try:
      load_json(release_dir, 'content.json')
    except TypeError:
      return print_error('file'+json_path+' is invalid JSON (parse error)')
    return True

def validate_title_app(title, app):
  if not title and not app:
    return print_error('--title and --application both missing, either is required.')
  elif title and app:
    return print_error('both --title and --application are specified, not only one.')
  return True



#------------------------------------------------------------------------------
# Main Entry
#------------------------------------------------------------------------------
if __name__ == '__main__':
  # Parse arguments
  parser = argparse.ArgumentParser(
    description="Automatically create/upload packages and releases.",
    formatter_class=lambda prog: argparse.HelpFormatter(prog,max_help_position=50))
  parser.add_argument(
    '--display-name', dest='display_name', type=str,
    help='(required) custom package display name; e.g. build number / tag. Unused if --package is specified')
  parser.add_argument(
    '--release-dir', dest='release_dir', type=str,
    help='(required) path to release content directory')
  parser.add_argument(
    '--build-dir', dest='build_dir', type=str,
    help='(required) path to build directory to deploy. Unused if --package is specified')
  parser.add_argument(
    '--title', dest='title',
    help='title ID. Use this arg to deploy to a title in "Publish". Incompatible with --application.')
  parser.add_argument(
    '--application', dest='app',
    help='title ID. Use this arg to deploy to a release for develop/QA. Incompatible with --title.')
  parser.add_argument(
    '--package', dest='pkg',
    help='package ID. Skip package creation and use this packageID.')
  parser.add_argument(
    '--intermediate-dir', dest='int_dir', default='./intermediate',
    help='directory used to store temporary intermediate files; default: ./intermediate')
  parser.add_argument(
    '--dry-run', dest='dry', default=False, action='store_true',
    help='print commands but do not run them')
  parser.add_argument(
    'game_args', metavar='args', type=str, nargs='+',
    help='command-line and arguments for running your game. Unused if --package is specified')
  opt = parser.parse_args()

  # Validate arguments
  success = True
  success = validate_build(opt.pkg, opt.build_dir, opt.game_args) and success
  success = validate_display_name(opt.pkg, opt.display_name) and success
  success = validate_release_dir(opt.release_dir) and success
  success = validate_title_app(opt.title, opt.app) and success

  # Run
  if success:
    exit(run_deployment())
  else:
    exit(255)
