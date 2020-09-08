#!/usr/bin/python
#
# @author      Oli Legat <olegat@google.com>
# @copyright   Google Inc
# @brief       Automatically download, update packageID and upload release tars
# @usage       Run with --help

import glob
import json
import optparse
import os
import shutil
import subprocess
import sys
import tarfile

# Org IDs:
#   Public Releases  92dff962466d4f7db70233fbfcea58f0rop1
#   Partner Eng      1f8754fe640c4741b43b62401113dceepup1
#
public_release_org_id = "92dff962466d4f7db70233fbfcea58f0rop1"
opt = None


#==============================================================================
#  Utility
#==============================================================================
def ggp(*args):
    # Open ggp.exe subprocess and read output/err
    ggp_args = ["ggp", "-s"] + list(args)
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
    global opt
    if opt.debug_ggp:
        print " ".join(ggp_args)
        print str(stdout)
    if stdout:
        return json.loads(stdout)
    else:
        return None

tmp_dir = None
def get_tmp_dir():
    global tmp_dir
    if not tmp_dir:
        tmp_dir = os.getcwd() + "/tmp"
    return tmp_dir

def makedirs(dirs):
    if not os.path.exists(dirs):
        os.makedirs(dirs)

def pick_package(package_list):
    for package in package_list: # package_list is a JSON array
        if package["status"] == "READY":
            return package
    return None

def pick_live_release(release_list):
    for rel in release_list: # release_list is a JSON array
        if rel["state"] == "LIVE":
            return rel
    return None

def download_release(application, release, path):
    ggp(
        "application", "release", "download",
        "--application", application,
        "--version", release['version'],
        "--path", path
    )
    # TODO(olegat) - File b/ to make ggp output the tar path.
    # There's no robust way to get the path of the downloaded tar,
    # because "ggp -s application release download" returns an empty string.
    # This scripts assumes that tmp doesn't exist at the start of the
    # execution and validates that assumption, so we can safely assume that
    # there is only 1 *.tar file in the tmp directory.
    # We only use download_release with the tmp dir, so we're good for now.
    return glob.glob(path + "/*.tar")[0]

def extract_release(tar_path, extract_path):
    makedirs(extract_path)
    tar = tarfile.open(tar_path)
    tar.extractall(extract_path)

def upload_release(application, path):
    ggp(
        "application", "release", "upload",
        "--application", application,
        "--path", path
    )
    

def load_json(content_dir, filename):
    with open(content_dir + '/' + filename) as f:
        return json.load(f)

def dump_json(content_dir, filename, content_json):
    with open(content_dir + '/' + filename, 'w') as f:
        return json.dump(content_json, f, indent=2)
    

#==============================================================================
#  Program Flow
#==============================================================================
"""
@brief Automatically select a package and release.

This function automatically selects the newest "READY" package and the newest 
"LIVE" release using the ggp command-line. This functions prints updates to
stdout.
@param application Application ID or display name.
@return A tuple (pkg, rel) where pkg is the package and rel is the release,
both are JSON objects decoded from the CLI output. pkg will be None if the
--package option is used.
"""
def pick_release_and_package(application):
    global opt
    pkg = None
    if opt.pkg:
        print "Skipping package listing (using explicit id "+opt.pkg+")"
    else:
        print "Listing packages."
        json_packages = ggp( "package", "list" )
        pkg = pick_package( json_packages )
        if pkg == None:
            print >> sys.stderr, "Error: no suitable package found"
            return (None, None)
        else:
            description = '"'+pkg['displayName']+'" (id: '+pkg['id']+')'
            print 'Using package '+description+' created at ' + pkg['created']

    print "Listing releases."
    json_releases = ggp(
        "application", "release", "list", "--application", application)
    rel = pick_live_release( json_releases )
    if rel == None:
        print >> sys.stderr, "Error: no suitable release found"
        return (None, None)
    else:
        print 'Using release '+rel['version']+'; live at ' + rel['live']

    return (pkg, rel) # success

"""
@brief Download, extract, update and archive the new release tar ball.

This downloads the specified release and then extracts the tarball content in
`get_tmp_dir() + "/extracted"`. The content.json's package ID is update and
the release archive is re-created. This functions prints updates to stdout.
@param application Application ID or display name.
@param release Release object (decoded JSON output), this is the release to
download and extract..
@param package Package object (decoded JSON output), the ID of this package
will be used in the new release archive.
@return The path to the new release archive.
"""
def create_updated_release(application, release, package):
    global opt
    # Download and extract release
    print 'Downloading release.'
    tmp = get_tmp_dir();
    extracted = tmp + "/extracted"
    old_tar_path = download_release(application, release, tmp)
    extract_release(old_tar_path, extracted)

    # Updated JSON.
    content = load_json( extracted, "content.json" )
    old_id = content["application_content"]["package"]["package_id"]
    new_id = opt.pkg
    if new_id == None:
        new_id = package["id"]
    content["application_content"]["package"]["package_id"] = new_id
    dump_json( extracted, "content.json", content )
    print 'Updated JSON package ID (old:'+old_id+' new:'+new_id+').'

    # Create new archive
    ggp("application", "release", "create-image",
        "--content", extracted,
        "--image", tmp+"/new_release.tar")
    return tmp+"/new_release.tar"

"""
@brief Run the entire script update.
@return The exit code for the script.
"""
def update_release_package_id(account, org, project, application):
    global opt
    print "Running ggp init."
    ggp("init",
        "--account="+account,
        "--organization="+org,
        "--project="+project
    )

    (pkg, rel) = pick_release_and_package( application )
    if rel == None:
        return 255
    if pkg == None and opt.pkg == None:
        print >> sys.stderr, "No package ID found or specified"
        return 255

    tarpath = create_updated_release( application, rel, pkg )

    if opt.upload:
        print "Uploading new release."
        upload_release(application, tarpath)
    else:
        print "Skipping release upload -- new release archive created: "
        print "\n    " + tarpath + "\n"

    print "Done."
    return 0


#==============================================================================
#  Program Entry
#==============================================================================
if __name__ == "__main__":
    # Parse args
    parser = optparse.OptionParser()
    parser.add_option("--account", dest="account",
                      help="partner account to use for this command (required)")
    parser.add_option("--application", dest="app",
                      help="application ID or display name (required)")
    parser.add_option("--project", dest="proj",
                      help="project ID or display name (required)")
    parser.add_option("--organization", dest="org", default=public_release_org_id,
                      help="organization ID (default: Public Releases)")
    parser.add_option("--package", dest="pkg",
                      help="new package ID (default: newest)")
    parser.add_option("--force", action="store_true", dest="force", default=False,
                      help="delete (or overwrite) any existing temp files")
    parser.add_option("--no-upload", action="store_false", dest="upload", default=True,
                      help="don't upload the updated release (implies --keep-tmp)")
    parser.add_option("--keep-tmp", action="store_true", dest="keeptmp", default=False,
                      help="don't delete temp dir when finished")
    parser.add_option("--debug-ggp", action="store_true", dest="debug_ggp", default=False,
                      help="(debug) print 'ggp -s' output")

    (opt, args) = parser.parse_args()

    # Implict options:
    if not opt.upload:
        opt.keeptmp = True

    # Validate args
    error = False
    if opt.account == None:
        print >> sys.stderr, "error: account not defined"
        error = True        
    if opt.proj == None:
        print >> sys.stderr, "error: project not defined"
        error = True
    if opt.app == None:
        print >> sys.stderr, "error: application not defined"
        error = True

    if error:
        print >> sys.stderr, "Try --help."
        exit(255)

    # Validate tmp directory
    tmp = get_tmp_dir()
    if os.path.exists(tmp):
        if opt.force:
            shutil.rmtree(tmp)
        else:
            print >> sys.stderr, "Temp directory "+tmp+" cannot be used because file already exists."
            print "This is probably because the script was run but did not exit cleanly. To resolve this, delete this file and run the script with --force."
            exit(255)

    # Run automation
    status = update_release_package_id(
        opt.account,
        opt.org,
        opt.proj,
        opt.app
    )
    if not opt.keeptmp:
        shutil.rmtree(tmp)
    exit(status)
