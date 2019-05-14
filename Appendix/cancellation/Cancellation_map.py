import re
import sys

for line in sys.stdin:
  val = line.split(",")
  cancel = int(val[21])
  if cancel == 1:
    print "%s\t%s\t%s" % ("mean", 1, 1)
  else:
    print "%s\t%s" % ("mean", 1, 0)