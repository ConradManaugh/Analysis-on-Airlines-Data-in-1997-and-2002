import sys

(last_key, cur_count, mean) = (None, 0, 0)
for line in sys.stdin:
  (key, count, valmean) = line.strip().split("\t")
  if last_key and last_key != key:
    print "%s\t%s\t%s" % (last_key, cur_count, mean)
    (last_key, cur_count, mean) = (key, int(count), float(valmean))
  else:
    first = cur_count * mean
    second = int(count) * float(valmean)
    total = cur_count + int(count)
    (last_key, cur_count, mean) = (key, total, (first+second)/total )

if last_key:
  print "%s\t%s\t%s" % (last_key, cur_count, mean)