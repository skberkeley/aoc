l1, l2 = [], []

with open('inputs/2024/full/day1') as f:
    for line in f:
        nums = line.split()
        l1.append(int(nums[0]))
        l2.append(int(nums[1]))

l2counts = {}

for n in l2:
    if n not in l2counts:
        l2counts[n] = 1
    else:
        l2counts[n] += 1

answer = sum([n * l2counts[n] if n in l2counts else 0 for n in l1])
print(answer)