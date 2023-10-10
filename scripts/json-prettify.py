import json
import sys
import os

if __name__ == "__main__":
  for path in sys.argv[1:]:
    with open(path, 'r') as ifile:
      obj = json.load(ifile);
    with open(path, 'w') as ofile:
      json.dump(obj, ofile, indent=2)
      del obj
