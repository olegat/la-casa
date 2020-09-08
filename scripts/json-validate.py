import json
import sys
import os

def parse(json_file):
  try:
    return json.load(json_file)
  except ValueError as e:
    print('invalid json: %s' % e)
    return None # or: raise

if __name__ == "__main__":
  for path in sys.argv[1:]:
    if os.path.isfile(path):
      with open(path) as f:
        parse(f)
    else:
      print('file ' + path + ' does not exist.')