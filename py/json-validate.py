import json
import sys
import os

def parse(text):
  try:
    return json.loads(text)
  except ValueError as e:
    print('invalid json: %s' % e)
    return None # or: raise

if __name__ == "__main__":
  for path in sys.argv[1:]:
    if os.path.isfile(path):
      parse(path)
    else:
      print('file ' + path + ' does not exist.')