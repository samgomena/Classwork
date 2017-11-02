# import numpy as np

table = []

for i in range(256):
  table.append(0)

for dividend in range(256):
  remainder = dividend

  for bit in range(8):
    if remainder & 0x80:
      remainder = (remainder << 1) ^ 0xD5
    else:
      remainder = (remainder << 1)

    
  table[dividend] = remainder

print table
